## Client

A demonstration on how server-client can be used for generating client
functions against the Reminders API.

To make this Client library easier to use by the consumers, all API functions
run in IO - by running *runClientM* under the hood - and are re-exported with
a modified name.

## RUN

To run the example in this package, first start the *server* (see README in server package)
and then execute:

```
stack exec client
```
