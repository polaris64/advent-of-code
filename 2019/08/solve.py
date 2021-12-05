import math

def split_image_layers(inp, w, h):

	# Calculate the number of layers and assert that it's an integer
	layers = len(inp) / (w * h)
	assert float(int(layers)) == layers

	# Return w*h-sized slices, one for each layer
	layer_len = w * h
	return [inp[x * layer_len:(x * layer_len) + layer_len] for x in range(0, int(layers))]

def get_image_checksum(image):

	# Count number of 0 pixels in each layer
	counts = [len([px for px in layer if px == 0]) for layer in image]

	# Get the index of the layer with the smallest number of 0 pixels
	layer_with_min_0 = min(enumerate(counts), key=lambda x: x[1])[0]

	# Count the number of pixels with values 1 and 2 in this layer: the
	# checksum is the product of these two counts
	return (
		len([px for px in image[layer_with_min_0] if px == 1]) *
		len([px for px in image[layer_with_min_0] if px == 2])
	)

def decode_image(image, w, h):

	# Start with a completely transparent output image
	output = [2] * w * h

	# Loop through layers, top to bottom
	for layer_idx in range(len(image)):

		# Copy each pixel only if the output is transparent at this
		# location
		for idx in range(0, w * h):
			if output[idx] == 2:
				output[idx] = image[layer_idx][idx]

	return output

def render_pixels(pixels, w, h):
	def pixel_to_char(px):
		return "#" if px == 1 else " "

	# Return scanlines joined by newlines
	return "\n".join([

		# Create string for all scanlines [0..h)
		"".join(

			# Convert each pixel to a char in this scanline
			[pixel_to_char(pixels[(y * w) + x]) for x in range(0, w)]

		) for y in range(0, h)
	])


def get_input():
	with open("input.txt", "rt") as f:
		return [int(x) for x in list(f.read().strip())]

def step1(inp, w, h):
	image = split_image_layers(inp, w, h)
	return get_image_checksum(image)

def step2(inp, w, h):
	image = split_image_layers(inp, w, h)
	pixels = decode_image(image, w, h)
	return render_pixels(pixels, w, h)


def main():
	dims = (25, 6)

	inp = get_input()
	print("Image checksum: {}".format(step1(inp, dims[0], dims[1])))

	output = step2(inp, dims[0], dims[1])
	print("Decoded image: -\n{}".format(output))


def test_example_step1():
	res = step1([int(x) for x in list("123456789012")], 3, 2)
	assert res == 1

	res = step1([int(x) for x in list("100200322411500600")], 3, 2)
	assert res == 4

def test_example_step2():
	output = step2([int(x) for x in list("0222112222120000")], 2, 2)
	assert output == " #\n# "

def test_step1():
	res = step1(get_input(), 25, 6)
	assert res == 1820

def test_step2():
	expected  = "#### #  # #  #  ##    ## \n"
	expected += "   # #  # # #  #  #    # \n"
	expected += "  #  #  # ##   #       # \n"
	expected += " #   #  # # #  #       # \n"
	expected += "#    #  # # #  #  # #  # \n"
	expected += "####  ##  #  #  ##   ##  "
	
	output = step2(get_input(), 25, 6)
	assert expected == output


if __name__ == "__main__":
	main()
