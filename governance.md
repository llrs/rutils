# Governance

This document describes the different roles for participants to the repository/project.

## Roles

At any point in time, a person is either a user, **Users**, a **Collaborator** or a **Publisher**.
Last two persons, Collaborators and Publishers, are known as members.
The list of Collaborators and Publishers is recorded in `members.yml` in this repository.

The contents of `members.yml` is used by the software managing the configuration of the repository/project.

## Rights and responsibilities

Each persona has the previous rights and responsibilities:

-   **Users**: can use the code according to the license of the repository and any additional file to this regard.\
    Can propose changes as specified by the license.
-   **Collaborators**: can update the code of the repository made by others and triage issues, comments or support publisher in other tasks.\
    They can process pull request and issues.
-   **Publishers**: can publish new releases and merge their own changes.\
    They administer the repository and, if exists, the organization following the principles outlined in this document.

## Onboarding

A User can apply to become a **Collaborator** by opening a pull request making the corresponding change in `members.yml`.
It is approved or rejected following a vote open to existing **members**.

\
A **Collaborator** can apply to become a **Publisher** by opening a pull request making the corresponding change in `members.yml`.
It is approved or rejected following a vote open to existing **Publishers**.

## Voting procedure

Eligible voters can cast three votes explaining their rationale:

-   Support: +1

-   Accept: 0

-   Reject: -3

Votes are cast publicly by commenting on the pull request.
The vote passes if the sum of votes is nonnegative.
Voting period is at least a week and up to the approval or rejection is mathematically secure.

## Offboarding

A **Collaborator** can step down to being a **User** and a **Publisher** can step down to being a **Collaborator** by making the appropriate pull request.
The pull request is accepted without a vote if it is opened by the person subject to the change.
A **Collaborator** who hasn't made use of **Collaborator's** rights for 2 years becomes a **User**.

A **Publisher** who hasn't made use of **members'** rights for 1 year becomes a **Collaborator**, unless there is only one **Publisher**.\

## Changing this document

Changes to this document are made by pull requests, where a vote open to **members** is held.
The person proposing the change is able to cast a vote following the above procedure.

## Known issues

Domains, tokens, accounts and other assets for the success of this project tied to a personal account of a **member**.
A solution to share its ownership with all Publishers will be welcome.

When configuration file isn't updated and then members use right's that shouldn't exercise.

When there's only one **Publisher** and does not keep the project working.

This document is available under a [CC0 license](https://creativecommons.org/public-domain/cc0/).
No rights reserved.
