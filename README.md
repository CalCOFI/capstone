# CalCOFI22
Capstone project for CCDSP fellowship, where we will store data and code
Data sets visual from Erin:
![image](https://user-images.githubusercontent.com/30590837/149233121-d5e2e83e-b72a-41e5-9e83-00ef40877b43.png)
## Basic Basics
* `git clone <url>` make a local copy of the pipeline
* `git pull`: sync local copy with remote copy
* `git add </path/to/file>` add changes in file to current commit-tracked changes
* `git commit` commit all tracked changes as one "unit" of work
* `git push` sync local commits with remote host
## Branching
* `git branch` list all branches
* `git checkout -b <new name>` make and switch local to a new branch
* `git merge <branch>` merge <branch> into current branch
* `git checkout <branch>` switch local directory to <branch>
* `git push --set-upstream origin <branch>` Creates your local branch in the remote repository
## Other useful stuff
* `git log` print list of commits with hashes
* `git checkout <hash>` go back in time and switch to a particular hash
* `git status` Check which files have been changed on this branch since the last commit
* `git diff <filename>` look at the changes you've made since the last commit
* `git stash`/`git stash save` put your uncommitted/unstaged changes away in a git provided stash so that you can switch to working on something else
* `git stash apply` to apply the changes you stashed. Works in any local branch, no matter where you saved the changes.
* `git checkout -- <filename>` to delete ALL of the changes on your branch since the last commit.
You can make pull requests for branches on the github app.
## Manually resolving conflict
Sometimes, when merging, pulling, pushing, or doing any version control, GitHub will not like you committing after applying changes. Here we have an example:
```
<<<<<<< HEAD
    <link type="text/css" rel="stylesheet" media="all" href="style.css" />
=======
    <!-- no style -->
>>>>>>> master
```
The **<<<** indicates the parts of the **original** file that are in conflict <br>
The **>>>** indicates the **changes you made** that are in conflict with the original file <br>
The **==** separates the two <br>
To fix this, you will have to manually edit the file so that your changes are included and work. Remove the <<<, ==, and >>> lines after you are done. Then the commit will work.
