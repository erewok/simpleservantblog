# Simple Servant Blog

- This is a work in progress: an attempt to build
a CMS-style thing out of Servant. It's mostly for
personal interest, so it may never be finished, but it's pretty fun to work on.


## ToDo

- ~~Implement routing and back button.~~
- ~~Fix syntax highlighting.~~
- ~~Build CMS-stuff: easy post-entry~~
- Build User-add, User-edit admin stuff
- Make admin Get all posts. Not just published ones.
- Fix date-inputter which formats and frustrates editing attempts.
- ~~Make elm-admin bundle~~
- ~~Make Posts FK to User table, not author, Or use author as one-to-one with User~~
- ~~Write create-tables script and add to cabal for output~~
- ~~Implement series list page.~~
- Add "prev/next" links for regular posts.
- Implement media associated with posts.
- ~~Add JS/static assets build pipeline~~
- Write tests for Backend
- Write tests for frontend
- ~~Build session handling (for admin work)~~
- ~~Begin working on dockerizing application~~
- Nginx + Postgresql in docker-compose with shared volume for assets.
- TLS Cert Auto-Renewal
- Add GA to website


## Intended Series on building this thing
1. Getting Started: Postgresql, Haskell, Stack, Elm, and project source
2. Backend Setup: Tables and Models
3. Creating an API using Servant
4. Serving raw assets and HTML with Servant
5. Frontend with Elm
6. Docker, Nginx, and Deploying your Haskell Application
7. Authentication, Authorization, and Session-less Sessions
8. Moving Toward a more full-fledged CMS
