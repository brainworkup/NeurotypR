"""
pip install outlines torch==2.4.0 transformers accelerate typing-extensions pillow pdf2image rich requests

may need to install tkinter: https://stackoverflow.com/questions/25905540/importerror-no-module-named-tkinter

sudo apt-get install poppler-utils
"""

from enum import Enum
from io import BytesIO
from PIL import Image
from urllib.request import urlopen
import outlines
import torch
from transformers import (
    LlavaForConditionalGeneration,
)
from pydantic import BaseModel, Field, confloat, constr
from pydantic.types import StringConstraints
from typing import List
from typing_extensions import Annotated

from pdf2image import convert_from_path
import os
from typing import List, Optional
from rich import print

import requests

model_name="mistral-community/pixtral-12b" # original magnet model is able to be loaded without issue
model_class=LlavaForConditionalGeneration

model_kwargs = {
    "torch_dtype": torch.bfloat16,
    "device_map": "auto",
}
processor_kwargs = {
    "device": "cuda",
}

model = outlines.models.transformers_vision(
    model_name,
    model_class=model_class,
    model_kwargs=model_kwargs,
    processor_kwargs=processor_kwargs,
)

def convert_pdf_to_images(
    pdf_path: str,
    output_dir: Optional[str] = None,
    dpi: int = 20,
    fmt: str = 'PNG'
) -> List[Image.Image]:
    """
    Convert a PDF file to a list of PIL Image objects.

    Args:
        pdf_path: Path to the PDF file
        output_dir: Optional directory to save the images
        dpi: Resolution for the conversion (200 is good for vision models)
        fmt: Output format (PNG recommended for quality)

    Returns:
        List of PIL Image objects
    """
    # Convert PDF to list of images
    images = convert_from_path(
        pdf_path,
        dpi=dpi,
        fmt=fmt
    )

    # Optionally save images
    if output_dir:
        os.makedirs(output_dir, exist_ok=True)
        for i, image in enumerate(images):
            image.save(os.path.join(output_dir, f'page_{i+1}.{fmt.lower()}'))

    return images

# Download the louf-willard pdf
# https://arxiv.org/pdf/2307.09702

# Download the PDF file
pdf_url = "https://arxiv.org/pdf/2307.09702"
response = requests.get(pdf_url)

# Save the PDF locally
with open("louf-willard.pdf", "wb") as f:
    f.write(response.content)

# Load the louf-willard pdf
images = convert_pdf_to_images("louf-willard.pdf", dpi=80, output_dir="output_images")

class PageSummary(BaseModel):
    description: str
    key_takeaways: List[str]
    page_number: int

page_summary_generator = outlines.generate.json(model, PageSummary)

instruction = f"""
<s>[INST]
You are an expert at summarizing pages from a scientific paper.
Please summarize the page.

Your schema is:
{PageSummary.model_json_schema()}

Please extract
[IMG][/INST]
""".strip()

for image in images:
    page_summary = page_summary_generator(instruction, [image])
    print(page_summary)