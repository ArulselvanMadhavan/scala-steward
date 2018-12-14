/*
 * Copyright 2018 scala-steward contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalasteward.core.dependency

import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import org.scalasteward.core.application.Config
import org.scalasteward.core.git.{GitAlg, Sha1}
import org.scalasteward.core.github.GitHubApiAlg
import org.scalasteward.core.github.data.{BranchOut, Repo, RepoOut}
import org.scalasteward.core.sbt.SbtAlg
import org.scalasteward.core.util
import org.scalasteward.core.util.MonadThrowable
import org.scalasteward.core.util.logger.LoggerOps

class DependencyService[F[_]](
    implicit
    config: Config,
    dependencyRepository: DependencyRepository[F],
    gitHubApiAlg: GitHubApiAlg[F],
    gitAlg: GitAlg[F],
    logger: Logger[F],
    sbtAlg: SbtAlg[F]
) {
  def forkAndCheckDependencies(repo: Repo)(implicit F: MonadThrowable[F]): F[Unit] =
    logger.attemptLog_(s"Fork and check dependencies of ${repo.show}") {
      for {
        res <- gitHubApiAlg.createForkAndGetDefaultBranch(repo)
        (repoOut, branchOut) = res
        refreshRequired <- decideAndRefreshDependencies(repo, branchOut)
        _ <- {
          if (refreshRequired) refreshDependencies(repo, repoOut, branchOut.commit.sha)
          else F.unit
        }
      } yield ()
    }

  private def decideAndRefreshDependencies(repo: Repo, branchOut: BranchOut)(
      implicit F: MonadThrowable[F]): F[Boolean] =
    for {
      foundSha1 <- dependencyRepository.findSha1(repo)
      latestSha1 = branchOut.commit.sha
      refreshRequired = foundSha1.fold(true)(_ =!= latestSha1)
    } yield refreshRequired

  def checkDependenciesWithoutForking(repo: Repo)(implicit F: MonadThrowable[F]): F[Unit] =
    logger.attemptLog_(s"check dependencies without forking ${repo.show}") {
      for {
        repoOut <- gitHubApiAlg.getRepoInfo(repo)
        branchOut <- gitHubApiAlg.getDefaultBranch(repoOut)
        refreshRequired <- decideAndRefreshDependencies(repo, branchOut)
        _ <- {
          if (refreshRequired)
            refreshDependenciesForNonForkedRepo(repo, repoOut, branchOut.commit.sha)
          else F.unit
        }
      } yield ()
    }

  private def cloneRepo(repo: Repo, repoOut: RepoOut)(implicit F: MonadThrowable[F]): F[Unit] =
    for {
      _ <- logger.info(s"Refresh dependencies of ${repo.show}")
      cloneUrl = util.uri.withUserInfo(repoOut.clone_url, config.gitHubLogin)
      _ <- gitAlg.clone(repo, cloneUrl)
    } yield ()

  private def updateDependencies(repo: Repo, latestSha1: Sha1)(
      implicit F: MonadThrowable[F]): F[Unit] =
    for {
      dependencies <- sbtAlg.getDependencies(repo)
      _ <- dependencyRepository.setDependencies(repo, latestSha1, dependencies)
      _ <- gitAlg.removeClone(repo)
    } yield ()

  private def refreshDependenciesForNonForkedRepo(repo: Repo, repoOut: RepoOut, latestSha1: Sha1)(
      implicit F: MonadThrowable[F]): F[Unit] =
    for {
      _ <- cloneRepo(repo, repoOut)
      // TODO: Think about switching to default branch.
      _ <- updateDependencies(repo, latestSha1)
    } yield ()

  private def refreshDependencies(repo: Repo, repoOut: RepoOut, latestSha1: Sha1)(
      implicit F: MonadThrowable[F]
  ): F[Unit] =
    for {
      _ <- cloneRepo(repo, repoOut)
      parent <- repoOut.parentOrRaise[F]
      parentCloneUrl = util.uri.withUserInfo(parent.clone_url, config.gitHubLogin)
      _ <- gitAlg.syncFork(repo, parentCloneUrl, parent.default_branch)
      _ <- updateDependencies(repo, latestSha1)
    } yield ()
}
