# Introduction

In the previous post in this series, we installed Postgresql 9.5 and set up a new user and a database. After that, we installed Haskell Stack and the Elm platform, and we cloned the repo for this project. If you have these tools on your system (and a local Postgresql with a database and user you can use), then you are ready to move onto this section, where will create our tables and start writing some Haskell to interact with them.


# Index

1. Getting Started: Postgresql, Haskell, Stack, Elm, and project source
2. Backend Setup: Tables and Models
3. Creating an API using Servant
4. Serving raw assets and HTML with Servant
5. A minimal frontend with Elm
6. Getting a bit fancier with Elm
7. Docker, Nginx, and Deployment
8. Authentication, Authorization, and Sessions without Sessions
9. Moving Toward a more full-fledged CMS


## Installing Dependencies

There are a handful of different ORMs for Haskell, including [Persistent](https://www.stackage.org/package/persistent) and [Opaleye](https://www.stackage.org/package/opaleye), but in an effort to keep things simple, we will be using a library called [postgresql-simple](https://www.stackage.org/package/postgresql-simple).

You should have `stack` installed on your system and in your PATH as well, and you should be in the root directory for this project. To get started, check out git branch `chapter2`:

```
$ git checkout chapter2
```
---
**Caveat**

Before we install dependencies, there is one large caveat here: I am on OSX and I have installed multiple Postgresql versions with `homebrew`, so my Postgresql 9.5 is in a non-traditional spot. As a result, in my `stack.yaml`, I have to include `extra-lib-dirs` in order to install a Postgresql binding called `libpq` and to have it find the right Postgresql 9.5 tools in my PATH.

There is a line in my `stack.yaml` that looks like this:

```
extra-lib-dirs: [/usr/local/opt/libiconv/lib, /usr/local/lib, /usr/lib]
```

**You will likely not need this**. My recommendation is to start by editing `stack.yaml` and commenting out this line:
```
# extra-lib-dirs: [/usr/local/opt/libiconv/lib, /usr/local/lib, /usr/lib]
```

If you are on OSX and you have installed stuff with `homebrew` and you're running into problems, you may have the same issue that I had, but I recommend researching it for solutions.
---

When you are ready to install project dependencies, run the following:

```
$ stack install --only-dependencies
```


## Working with Postgresql in Haskell

### Language Pragmas and Imports

After stack has installed our project dependencies, we can now start writing some code. Let's start with our model definitions.

We are working on a blog, but blog posts require authors, so let's start with our `Author` models first. We will begin with some useful GHC 8 language pragmas and then the imports our file requires.

*src/Models/Author.hs*:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Author (
  Author
  , authorColumns
  ) where

import Prelude (Int, Eq, Show, Bool, (.), ($))
import Control.Applicative ((<$>), (<*>))
import           Data.Aeson
import           Data.Maybe
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.FromRow (field, FromRow, fromRow)
import           Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics
import qualified Data.Text as T
import           Servant.Elm

```

Haskell modules always have their `module` names at the top: this one is called `Models.Author` because it is in the directory `Models` and it is named `Author.hs`. `Models.Author` is how this file will be referenced in our project's `cabal file` and it's also how it will be imported by other modules.

We have included a few imports from the `postgresql-simple` library we installed previously, and we also included some other interesting and useful tools:

**`Data.Aeson`**:

This is a wonderful JSON-serialization library. Seriously, it's the coolest JSON serialization tool I've encountered. We're going to rely heavily on `Data.Aeson`, but it is fast and has been battle-tested at scale by no less than [Facebook](http://www.serpentine.com/blog/2015/05/13/sometimes-the-old-ways-are-the-best/).


**`GHC.Generics`**:

We are going to create new data types to represent records in our tables. In other words, we're going to take a row from our database and turn it into something we are calling a `BlogPost` and then we're going to do the reverse: send that `BlogPost` directly back to the database where it will again become a record.

Our API is going to speak JSON, so it would also be cool to automatically turn these `BlogPost` and `Author` things into JSON and then back into `BlogPost` and `Author` things. `GHC.Generics` along with the language pragma `{-# LANGUAGE DeriveGeneric #-}` allows us to automatically `Aeson`'s '`FromJSON` and `ToJSON` instances for our data types, which simply means: the compiler automagically knows how to turn the things we define into JSON and from JSON back into the things we defined.

**`Servant.Elm`**:

We're not doing much with this yet, so we'll talk about it later. We'll include it for now.

### Defining our models

```haskell
authorColumns = "id, firstname, lastname, email"
data Author = Author {
  aid :: !Int
  , firstName :: !T.Text
  , lastName :: !T.Text
  , email :: !T.Text
  } deriving (Eq, Show, Generic)

instance ElmType Author
instance FromJSON Author
instance ToJSON Author
instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field <*> field


instance ToRow Author where
  toRow p =  [ toField $ aid p
             , fieldA firstName
             , fieldA lastName
             , fieldA email]
    where
      fieldA = toField . ($ p)

```


## Creating The Tables
