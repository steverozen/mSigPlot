"""Hash a PNG file using PIL + SHA-256 for regression testing."""

import hashlib
import sys

from PIL import Image

img = Image.open(sys.argv[1])
digest = hashlib.sha256(img.tobytes()).hexdigest()
print(digest)
