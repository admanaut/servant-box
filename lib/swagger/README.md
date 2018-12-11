## Swagger

This executable can generate Swagger JSON spec for our Reminders API that can
be used by Swagger UI to render the interactive documentation page.

To generate the JSON file, execute stack and redirect output to swagger.json

```
stack exec swagger > swagger.json
```

A better way of using Swagger is to serve the JSON spec under an endpoint in our API.
To see how this works please refer to the *server* package README file.
