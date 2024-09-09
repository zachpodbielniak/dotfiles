objects := 10-work.yaml
output := cr.yaml

all: $(output)

$(output): $(objects)
	echo "responses:" > $(output)
	for obj in $(objects); do yq -M .responses < $$obj >> $(output); done

clean: $(output)
	rm $(output)

