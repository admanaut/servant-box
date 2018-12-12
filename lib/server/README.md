## Server

A simple REST API for Reminders that exposes CRUD endpoints.

It's not connected to any persistance layer but it defines some default
reminders that we can use in our tests.

## Run

To run the server, after we built the binary, we need to run the following command:

```
stack exec server -- --port 8080

```

The executable expects a port number, this is so we can run it on Heroku
or if we have something else running on port 80.

## Swagger

Swagger spec is available at `/swagger.json`
