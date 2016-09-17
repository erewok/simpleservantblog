# Introduction

This is the beginning of a series of posts on creating this blog in Haskell.

There is not going to be much original content in here. Instead a lot of this material is available in other articles, blog posts, and repositories. Instead, this series merely demonstrates using one specific stack of components to follow a really common, well-worn path in the world of web applications: we are going to create a blog, a typical CRUD application, using Postgresql, Servant, and Elm.

The motivation in writing this series of posts is to highlight these interesting tools and show how certain common problems may be solved with them. No experience with Haskell should be necessary to follow along with this tutorial.

Haskell has a reputation for being an academic language, but it has lots to teach even lowly, everyday programmers such as the author of this series. Thus, we are writing this for an audience that may be otherwise intimidated by the theoretical approach and academic culture that thrives in the Haskell community. Haskell is certainly a diverse community with many people doing interesting work on the cutting edge of programming language theory; those people are pushing boundaries and their work is highly appreciated, but sometimes it can be nice to open the door to the people who want to get up and running quickly just to see what all of the hype is about and whether or not it holds up. This series is for those people.


### Caveats

Articles like this often go out of date over time. We are using a specific set of components here, and these will change and adapt. Many of the tools we use are part of quickly-moving ecosystems, so we apologize in advance if you come across this in the future and struggle because some of these components are old and their APIs have changed.

In addition, the author of this blog is a self-taught hacker type, someone who just plods along attempting to solve the specific problems he encounters. We make no attempt to argue that the solutions presented herein are the canonical, best answers to the some of the problems we encounter, and we encourage feedback if we err, which we likely will do, or if we misrepresent things, or suggest dangerous or careless practices.

Moreover, this series attempts to be a soup-to-nuts set of articles on building and deploying a Haskell/Elm application, and it covers so much territory that it will likely get stuff wrong.

Please file an issue on the [simpleservantblog repo](https://github.com/pellagic-puffbomb/simpleservantblog) with any problems you come across or with any suggestions or corrections.

Cheers.


# Index

1. Getting Started: Postgresql, Haskell, Stack, Elm, and project source
2. Backend Setup: Tables and Models
3. Creating an API using Servant
4. Serving raw assets and HTML with Servant
5. A minimal frontend with Elm
6. Getting a bit fancier with Elm
7. Docker, Nginx, and Deploying your Haskell Application
8. Authentication, Authorization, and Session-less Sessions
9. Moving Toward a more full-fledged CMS


## Components

We are going to start with the following tools:

- Stack resolver: nightly-2016-08-25 (GHC 8.0.1)
- Postgresql 9.5
- Servant 0.7.1
- Elm 0.17.0

# Getting Started

This section talks about installing and setting up Postgresql, a minimal Haskell toolchain with Stack, and the Elm Platform. Lastly, we will clone the [source code for this project](https://github.com/pellagic-puffbomb/simpleservantblog). If you already have or know how to get these things, you can skip to the next post in the series.

## Postgresql

We will be running a local copy of Postgresql. This series of posts is using Postgresql 9.5, which you can find at [postgresql.org](https://www.postgresql.org/).

Once you have it installed and running, you will need a database and a database user. There are lots of great tutorials on installing Postgresql for various operating systems, and these often include creating a new user and a new database. I encourage you to seek out a more comprehensive tutorial on these things for your specific operating system. Even so, what follows is a quick cheat sheet for creating a new user and a new database using Postgresql 9.5.

Locate your Postgresql installation directory and inside you should find the `bin` directory. If you add that directory to your PATH, we should be able to issue the following commands (swapping out `yourUserName` for the username you'd like to have):

```
createuser -P -s -e -d yourUserName
```

You should be prompted for a password, which you will need to remember to authenticate later on for this username.

Note: if `createuser` is not in your PATH, you will have to find the location where Postgresql has been installed on your system. Alternately, you may be able to open a shell with `psql` (as user `postgresql`) and issue SQL commands such as the following:

```
CREATE USER yourUserName with password 'PASSWORD';
```

After creating your user, you can now create a new database with the `createdb` command and make your user the owner of that database:

```
createdb -O yourUserName yourdatabase
```

Once again, substitute your username created above and a database name that you would like to use. We will be connecting to this database throughout the rest of this series.


## Haskell Stack

If you do not have minimal Haskell tooling on your computer, you will need `stack`. You can find instructions on installing it here: https://docs.haskellstack.org/en/stable/README/

### Text Editors

A footnote here on text-editors: the author of this series is using the [atom editor](https://atom.io/), which has excellent tooling for Haskell, Elm, CSS, Html, and others. If you have never edited Haskell source code, we recommend installing [atom](https://atom.io/) and then installing [ide-haskell](https://atom.io/packages/ide-haskell). Follow the `ide-haskell` instructions on installing the binary executables, and then install the other Atom/Haskell packages recommended by `ide-haskell`. Finally, start your text editor by navigating to the root directory of this project and running:

```
$ atom .
```

## Elm-Platform

The last tool you will need to follow along with this tutorial is Elm, which you can download and install here:

http://guide.elm-lang.org/get_started.html

## Repo

All code for this series is located here: https://github.com/pellagic-puffbomb/simpleservantblog

You can start by cloning this repo somewhere on your computer:

```
$ git clone https://github.com/pellagic-puffbomb/simpleservantblog
```

# Wrapup

In the next post in this series, we will create the tables and we will also define those tables in Haskell using the `postgresql-simple` library so that we may begin working with them.
