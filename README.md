
A crude prototype for a JSON transformation DSL using parser combinators.

It parses mapping descriptions into a function that takes a JSON document and
returns a new one.

## Building and running

Assuming you have a recent version of maven (>= 3.x) and JDK (>= 1.7.0)
installed, you can run the example mapping file and sample data with the
following:

```bash
mvn clean dependency:copy-dependencies package
java -cp 'target/dependency/*:target/*' prototype.jml.main test.jml sample?.json
```

The example transformation looks like this:

```yaml
# simple copying
data.original = input
data.subtype = input.type

# literals
data.type = "facebook"

# date parsing and formatting functions
data.created = input.created_time.or(env.now.asISO8601()).fromISO8601().asRFC2822()

# capture side-effects from the environment object
data.received = env.now.asRFC2822()

# complex ID generation
data.id = data.created.fromRFC2822().uuid(input.type, input.id.or(""), from.id.or(""))

# control flow with pattern matching
case data.subtype of {
  "like" =>
    # rely on the environment for some additional context
    data.content = input.from.name.concat("-likes-").concat(env.post.id)
  _ =>
    data.content = input.message.or(input.picture).or(input.link)
}
```

And the output should like something like the following:

```json
{
  "data":{
    "content":"Hello, world!",
    "id":"1e64b37df1eca900e01900003aed0799",
    "received":"Sat, 16 Jul 2016 13:15:54 +0100",
    "created":"Sat, 16 Jul 2016 10:30:02 +0100",
    "type":"facebook",
    "subtype":"photo",
    "original":{
      "type":"photo",
      "message":"Hello, world!",
      "created_time":"2016-07-16T09:30:02+0000",
      "from":{
        "name":"some-page",
        "id":"1234536895123123"
      },
      "link":"https://www.facebook.com/some-page/photos/80583996308305/10155201980523306/?type=3",
      "id":"12796398325_12125444480523307"
    }
  }
}
{
  "data":{
    "content":"some person-likes-42",
    "id":"1e64b4f0af97a900e019000001223e06",
    "received":"Sat, 16 Jul 2016 13:15:54 +0100",
    "created":"Sat, 16 Jul 2016 13:15:54 +0100",
    "type":"facebook",
    "subtype":"like",
    "original":{
      "type":"like",
      "from":{
        "name":"some person",
        "id":"11314536895123123"
      }
    }
  }
}
```
