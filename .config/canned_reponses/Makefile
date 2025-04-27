objects := 10-work.yaml 20-slack.yaml 50-code-bash.yaml 60-prompts.yaml 99-general.yaml
output := cr.yaml

all: $(output)

$(output): $(objects)
	echo "responses:" > $(output)
	for obj in $(objects); do yq -M .responses < $$obj >> $(output); done

clean: $(output)
	rm $(output)

