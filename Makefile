DOCKER_IMAGE := pandoc/core
IMAGE_VERSION := 2.7.3
REVEALJS_URL := https://cdn.jsdelivr.net/npm/reveal.js@3.8.0

watch:
	@$(MAKE) apresentação.html
	inotifywait -m -e modify,delete apresentação.org reveal_style.css code_style.css | while read; \
	  do \
	    $(MAKE) apresentação.html; \
	  done

apresentação.html: apresentação.org
	docker run --rm  -v $(CURDIR):/data $(DOCKER_IMAGE):$(IMAGE_VERSION) -t revealjs -s -o apresentação.html apresentação.org -V revealjs-url=$(REVEALJS_URL) -V theme=black --css reveal_style.css --css code_style.css --slide-level 2

dockerimage:
	docker pull $(DOCKER_IMAGE):$(IMAGE_VERSION)
