# piece-of-cake-slayer

![logo](https://user-images.githubusercontent.com/4276606/67370235-fc5cbb00-f582-11e9-9f45-09bf96ee6d0c.png)

[![GitHub CI](https://github.com/kowainik/piece-of-cake-slayer/workflows/CI/badge.svg)](https://github.com/kowainik/piece-of-cake-slayer/actions)

Template project for a web application based on the
[cake-slayer](https://github.com/kowainik/cake-slayer) architecture.

## How to build?

### Backend

Run the following command

```shell
cabal build
```

or

```
stack build
```

(it will take a while when run for the first time)

### Frontend

## How to run?

### Database

Run PostgreSQL database in one terminal window:

```
docker run -p 5432:5432 -e POSTGRES_PASSWORD=postgres postgres:12
```

### Backend

Run backend

```
cabal run piece-of-cake-slayer
```

### Frontend

Then open `localhost:8000` in your browser.

## Test

Testing backend via `curl`:

```shell
$ curl localhost:8080/items

$ curl -XPOST \
    -H 'Content-Type: application/json' \
    localhost:8080/createItem \
    -d '{"tag": "Item", "text": "New item"}'

$ curl -XPOST \
    -H 'Content-Type: application/json' \
    localhost:8080/deleteItem \
    -d '1'
```
