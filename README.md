# Gitdo

Gitdo is a Haskell program that creates Github issues from TODO comments in
your codebase. It provides two git hooks but can be used without.

Gitdo will scan the changed files after a commit and look for comments that
start with the word TODO. It will then update a Sqlite database. It is not
entirely brainless though; for example, if your changes cause the comment to
move down a few lines then the existing record will be updated - providing the
comment is still the same.

It started off as a mini project to see whether the
[Turtle library](http://www.haskellforall.com/2015/01/use-haskell-for-shell-scripting.html)
is a good replacement for scripting. Like all good scripts, the code I've
written is a mess - that may be my fault though! :D

I have started using it on this repository. However, it is not very smart at the
moment so issues may be closed or duplicated by changes to the corresponding
comment.

## Usage

To get started, install gitdo from Hackage:

```
$ cabal install gitdo
```

### With Hooks

Navigate to the directory you want to use gitdo in and run:

```
$ gitdo add-hooks
Created post-commit hook
Created pre-push hook
Created database
$
```

The final step is to configure the push hook. Open `.git/hooks/pre-push` in your
favourite text editor. You will find something like this:

```
#!/bin/bash
gitdo push -i IUSER -p PSWD -u USER -r REPO
```

Things in ALL CAPS need to be changed:

  * `IUSER` - the github user that will create any issues. 
  * `PSWD` - the password of `IUSER`
  * `USER` - the github user who owns the repo you will be pushing to
  * `REPO` - the github repository you will be pushing to

You can also set the `-c` flag. By default, gitdo will not close issues for TODO
comments that are missing. Setting `-c` enables this.

Now you are ready to go. Here is what it looks like:

```
$ git init .
Initialized empty Git repository in /home/matthew/Documents/example_project/.git/
$ gitdo add-hooks
Created post-commit hook
Created pre-push hook
Created database
$ touch test.py
$ git add .
$ # If you use gitdo on a new repository without any commits, on the first
$ # commit there will be an error. You can safely ignore it
$ # In the future, this won't happen!
$ git commit -m "Initial commit"
fatal: ambiguous argument 'HEAD^': unknown revision or path not in the working tree.
Use '--' to separate paths from revisions, like this:
'git <command> [<revision>...] -- [<file>...]'
[master (root-commit) 456cb8f] Initial commit
 1 file changed, 0 insertions(+), 0 deletions(-)
 create mode 100644 test.py
$ echo "# TODO: Actually write the bloody script you lazy half-wit" > test.py
$ git commit -am "Added a todo"
[NEW] test.py:1 Actually write the bloody script you lazy half-wit
[master 20adebf] Added a todo
 1 file changed, 1 insertion(+)
$ git push -u origin master
Username for 'https://github.com': GitdoBot
Password for 'https://GitdoBot@github.com': 
[SYNCED] test.py:1 Actually write the bloody script you lazy half-wit
Done
Counting objects: 6, done.
Delta compression using up to 4 threads.
Compressing objects: 100% (3/3), done.
Writing objects: 100% (6/6), 486 bytes | 0 bytes/s, done.
Total 6 (delta 0), reused 0 (delta 0)
To https://github.com/GitdoBot/test.git
 * [new branch]      master -> master
Branch master set up to track remote branch master from origin.
$
```

You can view the resulting repository
[here](https://github.com/GitdoBot/test). Note the issue!

### Without Hooks

If you don't want to use git hooks then you can just use the gitdo commands by
themselves.
