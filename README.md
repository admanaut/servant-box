# servant-box

a box of examples of how to use various servant libraries together

---

## Summary
In this repo I want to create a very simple REST API using [servant-server](http://hackage.haskell.org/package/servant-server) and then use some other servant libraries, like:

* [servant-client](http://hackage.haskell.org/package/servant-client) to generate a client library that we can distribute to consumers

* [servant-swagger](http://hackage.haskell.org/package/servant-swagger) to generate swagger specification which can then be used together with [swagger-ui](https://swagger.io/tools/swagger-ui/) to provide interactive documentation of the API

* [servant-quickcheck](http://hackage.haskell.org/package/servant-quickcheck) to test the API

And finally host the API and Swagger UI in Heroku.

## Servant server
A REST API built with servant-server.

## Servant client
Using server-client we generate all API functions and package them under
an easy to use library, ready to be distributed to consumers.

## Servant swagger
We can generate swagger spec via the swagger executable or request it from the API
via /swagger.json

### Swagger UI

Once we have access to the swagger spec we ca use Swagger UI to render it.

*locally*
`docker run -p 8081:8080 -e API_URL=http://localhost:8080/swagger.json swaggerapi/swagger-ui`

*Heroku*

TODO

## Servant quickcheck
TODO

## Build

Run `stack build` to build all packages in this project.

See README files in each package for more info.

## Heroku

*prereq: Heroku account and CLI*

We are going to use Heroku to host our API and Swwagger UI publicly.

The easiest way to get Heroku to run our Haskell binary is using Docker
with an image provided by FPCO. see *Dockerfile*

Because we're using Docker we need to login to Heroku's container registry first:

```
heroku container:login
```

We need to build the image and push it to Heroku

```
heroku container:push web --app servant-box-admanaut

```

Last step, release the app
```
heroku container:release web --app servant-box-admanaut
```

Optional, inspect logs
```
heroku logs --app servant-box-admanaut
```
