import torchvision
import torchvision.transforms as transforms
from PIL import Image
import os

os.makedirs("samples", exist_ok=True)

dataset = torchvision.datasets.MNIST(
    root="./data", train=False, download=True, transform=transforms.ToTensor()
)

saved = set()
for image, label in dataset:
    if label not in saved:
        img = transforms.ToPILImage()(image)
        img.save(f"samples/digit_{label}.png")
        saved.add(label)
        print(f"Saved digit_{label}.png")
    if len(saved) == 10:
        break
