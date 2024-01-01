Common Code Utilities by Tim De Baets  
Copyright © 2016-2024 Tim De Baets. All rights reserved.  
For conditions of distribution and use, see [LICENSE](LICENSE).  

Reporting issues
----------------

To report issues please use the project's issue tracker on GitHub. This should only be used to report actual bugs. Please **do not use the issue tracker for anything else**, such as feature requests or questions!

Contributing new code
---------------------

If you want to contribute new code to a project, don't clone its main repository, but create your own fork first and clone that fork instead. Then commit/push your work on a topic branch and submit a pull request. In more detail:

1. [Fork](https://help.github.com/articles/fork-a-repo/) the project.

2. Clone your fork. In a Windows command prompt window, run:
  ```
  > git clone https://github.com/YOUR_USERNAME/YOUR_FORK.git YOUR_FORK
  > cd YOUR_FORK
  > postclone.bat
  ```

3. Add an `upstream` remote: `git remote add upstream https://github.com/tdebaets/ORIGINAL_REPOSITORY.git`.

4. Get the latest changes from upstream: `update.bat upstream master`.

5. On GitHub, [create a new topic branch](https://help.github.com/articles/creating-and-deleting-branches-within-your-repository/) to contain your feature, change, or fix.

6. Checkout the new topic branch:
  ```
  > git fetch
  > git checkout TOPIC_BRANCH
  ```

7. While making changes to a file, please take care to follow the coding conventions currently used in the file: indentation, capitalization, function/parameter/variable naming, clarifying comments where needed, and so on. All indentation should be saved as spaces and never as tabs. Make sure that your text editor or IDE is configured to use spaces.

8. When you're done with your changes, check if the command-line compilation (`compile.bat`) still succeeds, even if compilation was already successful in the IDE. You are also expected to properly test your changes. Pull requests with buggy code won't be accepted.

9. Commit your changes to the topic branch: `git commit -a`.

10. Push your commit to the remote topic branch on your fork: `git push`.

11. Repeat steps 7-10 as many times as necessary, each time resulting in a new commit on the topic branch.

12. [Open a pull request](https://help.github.com/articles/using-pull-requests/) with a clear title and description.

### Keeping your topic branch up-to-date

To keep your topic branch up-to-date with the changes in the official repository, first make sure that you have either committed your changes or have stashed them using `git stash`. Then run `update.bat upstream master` again. Don't forget to run `git stash pop` afterwards if needed.

### Rebasing your topic branch on top of master

If new commits were made in the official repository before your pull request was merged, then your topic branch will have become outdated and you will probably be asked by the reviewer to rebase your topic branch on top of master. To do so, first run `update.bat upstream master` just like you would normally do to update your topic branch. The difference here however is that this rebases commits that you already pushed to your fork. In other words, you're 'rewriting history', and if you would subsequently try a normal `git push`, it would fail due to the push being non-fast-forward.

The correct way to proceed then is to force the push: `git push -f`. Usually, force pushing isn't recommended because it can cause you to lose commits. But in this case, you specifically want to get rid of the old, pre-rebase commits and replace them with the new, post-rebase commits. As a result, the topic branch on your fork will be overwritten with the state of the rebased topic branch, completely removing any record of the old branch. (This explanation is loosely based on [this StackOverflow answer](http://stackoverflow.com/questions/17182624/contributing-to-project-on-github-how-to-rebase-my-pull-request-on-top-of-mast/17182696#17182696).)
