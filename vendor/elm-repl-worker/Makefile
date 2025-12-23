
# Variables

DIST_WORKER=dist/worker.js


# Targets

.PHONY: worker clean

all: worker

worker: src
	elm make src/Repl/Worker.elm --output $(DIST_WORKER)

clean:
	rm -f $(DIST_WORKER)
