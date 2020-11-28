# Buildsystems


An important part of any software project is the build system.

## Use Stack

We use [stack](https://docs.haskellstack.org/en/stable/README/) to build the
haskell part of our system. Stack is well-established, professionally supported
and has the functionality we require.

Building the project is (for now) as simple as

```bash
stack build
```

## Use Docker

Having a built executable is not enough to run a service; for one thing we
usually need at least a database. Also, since we probably don't want to run the
production server on the development machine we need to ensure that we have a
consistent environment (avoiding the "works on my machine" problem). Docker
solves both issues; we write a
[Dockerfile](https://docs.docker.com/engine/reference/builder/) which defines
the environment our server will run in. And by using
[docker-compose](https://docs.docker.com/compose/compose-file/) we can
orchestrate the services we need in our system. So we can easily run the entire
stack on the development machine (for testing and debugging). And since we use
the same software stack on all machines (developers, servers), we can be
reasonably sure that incompatibilities won't crop up.

### Creating the Docker image

Since we are using Docker, the final built artifact of our software will be a
*Docker image*. There is multiple ways to create it. One way is to rely on
stack's built-in docker handling, and that works reasonably well in simple
cases. But customizing the result becomes complex quickly so that the advantage
over just using Dockerfiles diminishes or is even reverted.

#### Writing a Dockerfile

Instead, we write our own Dockerfile. It's not too complicated. We begin with a
`FROM` clause defining the image we start from (ubuntu in this example), a label
so we can identify images built from this dockerfile for cleanup, then
additional packages that need to be installed. Note that we combine shell
commands with `&&` rather than use multiple `RUN` instructions; this ensures
that we only produce one layer in the image with the final result of the script
and any temporary files don't end up in the image.

Then we configuring the locale and finally we add the executable and define it
as the entrypoint. (For more information please refer to the
[documentation](https://docs.docker.com/engine/reference/builder/)).

```Dockerfile
FROM ubuntu:eoan

LABEL project=nejlacommon-tutorial

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y --no-install-recommends  \
      ca-certificates \
      curl \
      libicu-dev \
      libpq-dev \
      locales \
      netbase  \
        && \
    rm -rf /var/lib/apt/lists/* && \
    localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

ENV LANG=en_US.UTF-8

ADD ./dist/* /app/

ENTRYPOINT ["/app/nejlacommon-tutorial"]
```

You will have noticed that we are adding the executable from the `dist`
directory. To make this work we need to instruct stack to place the executable
in there:

```bash
stack build --copy-bins --local-bin-path ./dist
```

now we can build the image:

```bash
docker build .
```

The output will be the image ID, which is a little inconvenient to work
with. Instead we want to *tag* the image, so we can refer to it by name:

```bash
docker build . -t nejlacommon-tutorial
```

The script to build our project is now

```bash
stack build --copy-bins --local-bin-path ./dist
docker build . -t nejlacommon-tutorial
```

Obviously, nobody can remember all of this, and even if we could nobody got time
to type it, which brings us to the next point:

## Use Make

Yes, little old make. But we don't want it as a replacement for stack, but just
to glue the various parts of our buildsystem together and serve as a repository
of scripts. This becomes even more convenient once we start adding more
components, say webpack for the frontend. Building the entire system should
always be as easy as typing `make`. Running the entire test suite can just be
`make test`. And if we want to build and run the service on our local machine
`make up` should accomplish that.

As a first approximation, a *Makefile* consists of a series of *rules*. Each
rule specifies a *target* (*what* is built), it's *dependencies* (what has to be
built before this rule can run) and a script that defines *how* to build that target.

Let's define a rule to build our project executable:

```Makefile
dist/nejlacommon-tutorial:
	mkdir -p dist
	stack build --copy-bins --local-bin-path ./dist
```

`dist/nejlacommon-tutorial` is the path to be built an also the name of the
rule. We don't have any dependencies (nothing after the colon). The built script
is simply the call to stack as before. We also explicitly create the dist
directory, that's not actually necessary, but it's a good idea to be explicit
about this.

Note that we need actual tabstops before the commands, multiple spaces will *not* work.

Running `make dist/nejlacommon-tutorial` will now build the executable. And
since this is the first (and only) rule, make will execute it when we only type
`make` without arguments. (Remember: put the default rule first). However, if
you modify the source and run `make` again you will notice that it won't get
recompiled. That's because make sees that the target file already exists and
since none of the dependencies have changed (there are none) make doesn't
recreate it. To fix this, we mark the rule by adding it as a dependency to the
special target `.PHONY`. This means that make doesn't see it as a file rule any
more and re-execute it every time . That may seem surprising since
`dist/nejlacommon-tutorial` actually *is* a file and we might expect to use file
rules. However, we don't want to manually add all the source files as
dependencies (potentially many), and we might forget to update it when we create
new source files. Also, stack is already doing the heavy lifting for us and
avoids recompiling source files if they haven't changed. (Actually, there is a
trick to adding all source files without manually enumerating them, and it might
be a good idea to employ it since calling stack can still be slow even if it
doesn't have to compile anything. Also, once we mark one target as .PHONY all
targets depending on it will *also* be re-executed every time. But for now this
is good enough and illustrates an important point: When "cheating" get's us 90%
of the result for 10% of the work (and learning makefile syntax definitely is
work), we will happily do it until we really do need the remaining 10%)

```Makefile
.PHONY: dist/nejlacommon-tutorial
dist/nejlacommon-tutorial:
	mkdir -p dist
	stack build --copy-bins --local-bin-path ./dist
```

Next we want to build the image, let's add a target for that. We add
`dist/nejlacommon-tutorial` as a dependency to ensure it gets built before we
build the image. We also mark it as `.PHONY`, since "image" is not a file we are
creating and we want to re-run it every time (relying on docker to cache build
steps for us):

```Makefile
.PHONY: image
image: dist/nejlacommon-tutorial
	docker build . -t nejlacommon-tutorial
```

Lastly, we want a target for cleanup:

```Makefile
.PHONY: clean
clean:
	stack clean
	rm -rf dist
	rm -f stack.yaml.lock
```

So the complete Makefile looks like this:

```Makefile
.PHONY: image
image: dist/nejlacommon-tutorial
	docker build . -t nejlacommon-tutorial

.PHONY: dist/nejlacommon-tutorial
dist/nejlacommon-tutorial:
	stack build --copy-bins --local-bin-path ./dist

.PHONY: clean
clean:
	stack clean
	rm -rf dist
	rm -f stack.yaml.lock
```

Make will execute the first target when we run `make` without an explicit
argument, so we put `image` first.

Now anyone who wants to build the project only needs to enter `make`.

## docker-compose

With an easy way to build the project, we now want to actually run it. To that
end we define a docker-compose file with a database service and our newly built
project:


```yaml
version: '3'

services:
  database:
    image: postgres:12.3
    environment:
      - POSTGRES_HOST_AUTH_METHOD=trust
  app:
    image: testproject
    environment:
      - APP_EMAIL=testuser@example.com
      - DB_HOST=database
      - DB_USER=postgres
    ports:
      # Forward local port 8080 to port 80 inside the container
      - 8080:80
```

(Please refer to the [docker-compose file
reference](https://docs.docker.com/compose/compose-file/))

We can start the service with

```bash
docker-compose -p nejlacommon-tutorial up -d
```

and stop it with

```bash
docker-compose -p nejlacommon-tutorial down
```

Let's add shortcuts for them to the makefile:

```Makefile
.PHONY: up
up: image
	docker-compose -p nejlacommon-tutorial up -d

.PHONY: down
down:
	docker-compose -p nejlacommon-tutorial down
```

`make up` will now re-build the project and start it. `make down` stops the running container.

Let's also update the `make clean` target to remove the containers, volumes and
images we created:

```Makefile
.PHONY: clean
clean:
	docker-compose -p nejlacommon-tutorial down -v
	docker image prune -f -a --filter="project=nejlacommon-tutorial"
	stack clean
	rm -rf dist
	rm -f stack.yaml.lock
```

`docker image prune` removes dangling (untagged) images, `-f` disables
confirmation, `-a` also removes tagged images and the `--filter` selects the
images belonging to our project. (Also refer to the
[documentation](https://docs.docker.com/engine/reference/commandline/image_prune/))
