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

Before we install dependencies, there is one large caveat here: I am on OSX and I have installed multiple Postgresql versions with `homebrew`, so my Postgresql 9.5 is in a non-traditional spot. As a result, in my `stack.yaml`, I have to include `extra-lib-dirs` in order to install a Postgresql binding called `libpq` so it can find the right Postgresql 9.5 tools in my PATH.

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

### First Models File: Author.hs

After stack has installed our project dependencies, we can start writing some code. It's always a good idea to begin by first considering the structure of our data, so in web apps we start with the model definitions. These will represent both the tables that store our data and the objects we interact with in our code.

We are working on a blog, but blog posts require authors, so let's start with our `Author` models first.

We begin our module with some useful GHC 8 language pragmas and then the imports our file requires.

*src/Models/Author.hs*:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Author (
  Author(..)
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
At the very top are some GHC language pragmas: these are extensions available in GHC, and we are going to gloss over them here, suffice it to say that these go *above* the name of our module and its imports.

Haskell modules always have the `module` name at the top below any pragmas: this module is called `Models.Author` because it is in the directory `Models` and it is named `Author.hs`. `Models.Author` is how this file will be referenced in our project's `cabal file` and it's also how it will be imported by other modules.

We have also included a few imports from the `postgresql-simple` library we installed previously, and we have included some other interesting and useful tools, including the following:

**`Data.Aeson`**:

This is a wonderful and extremely fast JSON-serialization library. Throughout this project, we're going to rely heavily on `Data.Aeson`.


**`GHC.Generics`**:

We are going to create some new data types to represent records in our tables. In other words, we're going to take a row from our database and turn it into something we are calling a `Author` and then we're going to do the reverse: send that `Author` directly back to the database where it will again become a database record.

In addition, our API is going to speak JSON, so it would also be pretty cool to automatically turn our `BlogPost` and `Author` things into JSON and them from JSON back into `BlogPost`s and `Author`s. `GHC.Generics` along with the language pragma `{-# LANGUAGE DeriveGeneric #-}` allows us to automatically create instances of `FromJSON` and `ToJSON` (from the `aeson` library) for our data types, which simply means that the compiler automagically knows how to turn the things we have defined *into* JSON and then from JSON *back into the things we defined*, which is pretty cool.

**`Servant.Elm`**:

We're not doing much with this yet, so we'll talk about it later. Simply include it for now.

### Defining the Author Table

The first thing we will do is define our `Author` data type as it will correspond to a record in the `author` table in our database.

*src/Models/Author.hs*:

```haskell

data Author = Author {
  aid :: !Int
  , firstName :: !T.Text
  , lastName :: !T.Text
  , email :: !T.Text
  } deriving (Eq, Show, Generic)
```
Our Author table will have four columns named similarly to the above, but in our application, we will generally be dealing with `Author` things, so we will to define how to get one of these `Author` things from our database for us and how to put it back. We do that by defining `FromRow` and `ToRow` (from the `postgresql-simple` library) instances for our object.

Note: the "bang pattern" (`!`) in our fields means that those fields are *strict*, not lazy. It forces evaluation of its field instead of leaving it to be evaluated later. This is recommended for data types such as `Int`, `Text`, and others. If this doesn't make much sense, don't sweat it. The code above will also work without it.

#### Our FromRow Instance

Creating a `FromRow` instance is pretty straightforward. We simply define the `fromRow` function by stating that there are a specific number of fields corresponding to columns in our table:

```haskell

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field <*> field
```

This `FromRow` instance says an `Author` object can be created by applying the `Author` value constructor to four fields (because our table has four columns). However, if we were to add another field to our `Author` record (and a column to the `author` table in our database), then the `fromRow` definition would be changed to look like this:

```haskell
data Author = Author {
  aid :: !Int
  , firstName :: !T.Text
  , lastName :: !T.Text
  , email :: !T.Text
  , a_new_field :: !T.Text
  } deriving (Eq, Show, Generic)

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field <*> field <*> field  -- An extra one added at the end
```

#### Our ToRow Instance

Going the other way, to create a `ToRow` instance, we have to describe how to take an `Author` and retrieve its data in order to send it back to the database as a row. The function we will create is called `toRow` and altogether it looks like this:

*src/Models/Author.hs*:

```haskell
instance ToRow Author where
  toRow p =  [ toField $ aid p
             , toField $ firstName p
             , toField $ lastName p
             , toField $ email p]
```

Here, we gather all of these pieces of data into a list and this list represents a row in the table.

#### Some Haskell Basics

A quick detour: a Haskell record such as our `Author` definition has what look like field-names, but these are really just functions. Calling these functions corresponds to accessing that piece of data in the record.

Here's a really basic introduction to Haskell records:

```
$ stack ghci
ghci> :set -XOverloadedStrings
ghci> import Models.Author
ghci> let me = Author 1 "erik" "aker" "erik@ekadanta.co"
ghci> > me
Author {aid = 1, firstName = "erik", lastName = "aker", email = "erik@ekadanta.co"}
```

After that, I can now call these functions on the `Author` object in order to get the data back out:

```
ghci> email me
"erik@ekadanta.co"
ghci> firstName me
"erik"
```

Thus, our `ToRow` instance is calling all of the functions necessary to get the data out of the object and after that each datum gets turned into a `field` via the `toField` function:

```haskell
...
   toField $ firstName p
...
```

If you are completely unfamiliar with Haskell, that `$` may look odd (and probably `<$>` and `<*>` above which I have ignored do too). The `$` simply means "apply this function to this argument". Another way to look at it is like this: `toField` is a function that takes one argument, but Haskell doesn't use parentheses for function calls, so when we write the following: `toField firstName p`, the compiler rejects this saying:

```haskell

Couldn't match expected type ‘Author
                                -> Database.PostgreSQL.Simple.ToField.Action’
              with actual type ‘Database.PostgreSQL.Simple.ToField.Action’
• The function ‘toField’ is applied to two arguments,
  but its type ‘(Author -> T.Text)
                -> Database.PostgreSQL.Simple.ToField.Action’
  has only one
```

In other words, we have accidentally applied the `toField` to two arguments instead of one: the compiler cannot disambiguate `toField firstName p` from what we *really* meant, which was to first call `firstName p` and then *apply* `toField` to the result of that.

There are two solutions to this problem of passing too many arguments, we can use parentheses to group things, or we can use `$` instead:

``haskell

   toField $ firstName p
-- the following is equivalent:
   toField (firstName p)
```

At any rate, once we have assembled all of the `field`s into a list, we now have a representation of a row in the `author` table.

Finally, we can now use generics to define our convenience JSON converters and an Elm one, which we'll use later to save ourselves a bunch of work in Elm. (If you would like to take the time to write the `FromJSON` and `ToJSON` instances by hand, I recommend reading [this excellent tutorial](https://artyom.me/aeson) on `aeson`.)

*src/Models/Author.hs*:

```haskell

instance ElmType Author
instance FromJSON Author
instance ToJSON Author
authorColumns = "id, firstname, lastname, email"
```

That last definition is a convenience for querying, which we'll also use later on too.

### Compiling with Stack Build

We have now completed one of the modules of our project, so let's try to build it:

```
$ stack build
```

If all goes well, you should see no compiler errors. If something went wrong, you will have to fix it before work can continue. Make sure that your module includes the code we've entered here. This is the agony and the ecstasy of working in Haskell: the compiler is extremely unforgiving, but it eventually becomes your best friend. After your modules compile, they are likely to work.

### Defining the Blog Post Tables

Now that we have a way of representing our `author` table, we'd like to create a `blogpost` table because each record there will be linked to a record in the `author` table.

What follows is very similar to what we've seen before:

*src/Models/Post.hs*:

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Post (
  , BlogPost(..)
  , blogPostColumns
  ) where

import Prelude (Int, Eq, Show, Bool, (.), ($), (++))
import Control.Applicative ((<$>), (<*>))
import           Data.Aeson
import           Data.Maybe
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.FromRow (field, FromRow, fromRow)
import           Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import           Data.Time (UTCTime)
import           GHC.Generics
import qualified Data.Text as T
import           Servant.Elm

data BlogPost = BlogPost {
  bid :: !Int
  , authorId :: !Int
  , title :: !T.Text
  , body :: Maybe T.Text
  , created :: !UTCTime
  , modified :: Maybe UTCTime
  , synopsis:: Maybe T.Text
  , pubdate :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field
            <*> field <*> field <*> field
            <*> field <*> field

instance ToRow BlogPost where
  toRow p =  [toField $ bid p
            , toField $ authorId p
            , toField $ title p
            , toField $ body p
            , toField $ created p
            , toField $ modified p
            , toField $ synopsis) p
            , toField $ pubdate p
          ]
```

There are two potentially interesting things in here. First, we have chosen to represent some of our fields in the following way: ` seriesId :: Maybe Int`. You may have guessed these are *nullable* fields.

Haskell will never let us put a `Null` in this field if it is defined to be an `Int`. Thus, I must define this field as a `Maybe Int`, in order to indicate that it can be either one of two things: a number wrapped in a constructor, `Just 5`, or the value `Nothing`. Any time I interact with this field I must explicitly check whether it is a `Just n` or `Nothing`. If you have explored Haskell before you have most likely come across the `Maybe` type: it's very useful.

Note: we didn't add bang patterns to the `Maybe` types. To see why, try to add one and `stack build` the project.

In addition, I have also defined some of my fields to be `UTCTime`: these correspond to Postgresql's `timestamp with time zone` data type.

Finally, we also derive our other instances and throw in the convenience column list, so our later queries will match up nicely with our `BlogPost` value constructor:

```haskell

instance ElmType BlogPost
instance FromJSON BlogPost
instance ToJSON BlogPost
blogPostColumns = "id, authorid, title, body, " ++
                  "created, modified, synopsis, pubdate"
```

After that module has been completed, again run `stack build`. If everything built successfully, we are ready to create our tables and put some data into them.


## Creating The Tables

We are now ready to create our tables and start querying them. Following are the SQL statements required to build the tables we have represented in code. Connect to your local Postgresql database and enter the following:

```sql

CREATE TABLE author (
  id serial primary key,
  firstname character varying(40) NOT NULL,
  lastName character varying(50),
  email character varying NOT NULL,
  CONSTRAINT unique_email_key UNIQUE (email)
);
CREATE TABLE post (
  id serial primary key,
  authorid integer NOT NULL,
  title character varying(255) NOT NULL,
  body text,
  created timestamp with time zone NOT NULL,
  modified timestamp with time zone,
  synopsis text,
  pubdate timestamp with time zone,
  CONSTRAINT author_id_post_fk FOREIGN KEY (authorid)
      REFERENCES public.author (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE
);
```

Let's use Haskell now to enter some data!

## Connecting and Querying

In order to connect to our database using `postgresql-simple`, we need to create a `Connection`. The a function `connectPostgreSQL` helps us create a connection. It has the following signature:

```haskell

connectPostgreSQL :: ByteString -> IO Connection
```

In order to use this function to create a `Connection`, we need a `ByteString` representing the Postgresql connection string. It looks like something this:

```haskell

"host=localhost port=5432 user=youUser password=yourUserPassword dbname=yourDbName"
```

We can create a `Connection` and write some queries in ghci in the following way:

```
$ stack ghci
ghci> :set -XOverloadedStrings
gchi> import Database.PostgreSQL.Simple
ghci> let connStr = "host=localhost port=5432 user=youUser password=yourUserPassword dbname=yourDbName"
ghci> conn <- connectPostgreSQL connStr
```
