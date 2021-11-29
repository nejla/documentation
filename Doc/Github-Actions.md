# Using github actions

* Main concepts: "Workflow", "Job", "Step"
* A "Workflow" is a set of jobs that should be run under certain conditions,
  that's to say it is a pipeline of for example builds, tests, publishing and
  deployment.
* Each "workflow" is defined a `.yaml` file under `.github/workflows`
* Workflows are triggered by certain events (`on` field), e.g. push or PR
* Each workflow consists of a list of jobs (e.g. build, test, upload), where each job can have a number of steps
* Jobs are independent unless specified otherwise
* Each job (not step) runs in a fresh virtual machine or a docker container -
  the same one for all the steps
* Default is an ubuntu 20.04 machine
* The ubuntu machine comes with a lot of tools pre-installed: [list of
  tools](https://github.com/actions/virtual-environments/blob/main/images/linux/Ubuntu2004-README.md)
* Can use artifacts to move files between jobs
* Steps can be shell commands
* Steps can alternatively use "actions", which are essentially pre-packaged
  scripts hosted on github
* Actions can have effects at different stages of the job, e.g. an action
  listed as an early step can run steps later in the pipeline, this is how the
  "cache" action can restore the cache at the beginning of the run and record
  files at the end
* actions are identified by their github repository and a git reference in a `uses` field
* Example: [`docker/build-push-action`](https://github.com/docker/build-push-action) builds and tags a docker image and
  (optionally) publishes it to a registry (or exports it to a file, see
  documentation)
* Actions can take arguments in `with` block to customize behaviour (See documentation of the action)
* Actions can generate "outputs" which are variables to be used in later steps
  (See [expression
  syntax](https://docs.github.com/en/actions/reference/context-and-expression-syntax-for-github-actions))
* /NB/: It's important to ensure that the action is trustworthy, either need to
  audit action and then pin commit SHA or use actions from trustworthy sources!


## Publishing to docker

### Restricting access on dockerhub
  * Create a "CI" or "bot" account on dockerhub
  * Create an access token for the account (Account settings -> Security
  * Make bot account collaborator on the respective repositories
  * In the github repo: Under Settings -> Secrets set up two secrets,
    `DOCKERHUB_USERNAME` and `DOCKERHUB_TOKEN` with the dockerhub bot account username and

### Set up the github action:
* Login to Dockerhub, use [`docker/login-action`](https://github.com/docker/login-action)

```yaml
      - name: Login to DockerHub
        uses: docker/login-action@v1
        with:
          username: ${{ secrets.DOCKERHUB_USERNAME }}
          password: ${{ secrets.DOCKERHUB_TOKEN }}
```
### Generating tags
* Use [`docker/metadata-action`](https://github.com/docker/metadata-action) to
  calculate the tags to generate (e.g. version or git SHA)
* Can e.g. create a docker tag for a corresponding docker tag, branch name or commit SHA

```yaml
      - name: Generate docker tags and labels
        id: docker-meta
        uses: docker/metadata-action@v3
        with:
          images: philonous/auth-service-test
          tags: |
            type=sha,format=long
            type=ref,event=branch
            type=ref,event=pr
            type=semver,pattern={{version}}
            type=semver,pattern={{major}}.{{minor}}
```

* Build and push an image with
  [`docker/build-push-action`](https://github.com/docker/build-push-action).
* Reference the tags and labels generated in the previous step. This makes use of
  the `id` field, so we can uniquely identify the step

```yaml
      - name: Build docker image
        uses: docker/build-push-action@v2
        with:
          context: service
          push: true
          tags: ${{ steps.docker-meta.outputs.tags }}
          labels: ${{ steps.docker-meta.outputs.labels }}
```
