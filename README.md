# Simple Servant Blog

- This is a work in progress: an attempt to build
a CMS-style thing out of Servant. It's mostly for
personal interest, so it may never be finished, but it's pretty fun to work on.


## ToDo

- Rewrite queries so the field ordering matches columns requested.
- Implement routing and back button.
- ~~Fix syntax highlighting.~~
- Implement series/series-index.
- Continue to work on styling.
- Write tests for Backend
- Write tests for frontend
- Begin working on dockerizing application
- Nginx + Postgresql in docker-compose with shared volume for assets.
- Write auto-loader for posts in docs/author when db is empty.
- Work on deployment with CircleCI + docker + digitalocean
- Build session-less session handling (for admin work)
- Build CMS-stuff: easy post-entry

## Intended Series on building this thing
1. Getting Started: Postgresql, Haskell, Stack, Elm, and project source
2. Backend Setup: Tables and Models
3. Creating an API using Servant
4. Serving raw assets and HTML with Servant
5. A minimal frontend with Elm
6. Getting a bit fancier with Elm
7. Docker, Nginx, and Deploying your Haskell Application
8. Authentication, Authorization, and Session-less Sessions
9. Moving Toward a more full-fledged CMS
