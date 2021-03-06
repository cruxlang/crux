# crux-closure-service

A fork of Closure Compiler designed to optimize code generated by the Crux compiler.

## Resources

- [Getting Started with Java on Heroku](https://devcenter.heroku.com/articles/getting-started-with-java) article - check it out.
- [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.png)](https://heroku.com/deploy)
- [Java on Heroku](https://devcenter.heroku.com/categories/java)

## Running Locally

Make sure you have Java and Maven installed.  Also, install the [Heroku Toolbelt](https://toolbelt.heroku.com/).

TODO: The following instructions are broken. closure-service/Procfile needs to
be updated to detect whether running from the root of the crux/ repo or not.

```sh
$ git clone https://github.com/heroku/java-getting-started.git
$ cd java-getting-started
$ mvn install
$ heroku local
```

Your app should now be running on [localhost:5000](http://localhost:5000/).

## Deploying to Heroku

Setup:

```sh
$ git remote add heroku https://git.heroku.com/crux-closure-service.git
$ heroku login
```

```sh
$ git push heroku master
```

## Heroku Configuration

```
$ heroku config
=== crux-closure-service Config Vars
BUILDPACK: closure-service=https://github.com/heroku/heroku-buildpack-java
PROCFILE:  closure-service/Procfile
$ heroku buildpacks
=== crux-closure-service Buildpack URLs
1. heroku/jvm
2. https://github.com/Pagedraw/heroku-buildpack-select-subdir
```
