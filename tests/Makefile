
args = `arg="$(filter-out $@,$(MAKECMDGOALS))"`


test:
	bash generate $(filter-out $@,$(MAKECMDGOALS))

.PHONY: test
