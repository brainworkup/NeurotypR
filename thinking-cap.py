import outlines
from transformers import AutoTokenizer

model_string = 'deepseek-ai/DeepSeek-R1-Distill-Qwen-7B'
# model_string = 'deepseek-ai/DeepSeek-R1-Distill-Qwen-1.5B' # For small machines

model = outlines.models.transformers(
    model_string,
    device='cuda', # also 'cpu', 'mps','auto'
)
tokenizer = AutoTokenizer.from_pretrained(model_string)

thinking_regex = r'<think>(.|\n){500}\n\[THINKING_TRUNCATED\]\n</think>(yes|no)'

prompt = tokenizer.apply_chat_template(
    [
        {'role': 'system', 'content': 'You are a helpful assistant.'},
        {'role': 'user', 'content': 'Roses are red. Violets are blue. Are roses and violets the same color? Yes or no.'},
    ],
    tokenize=False,
    add_generation_prompt=True,
)

# Generator 
generator = outlines.generate.regex(model, thinking_regex)
print("Generator created")

# Result 
result = generator(prompt)
print(result)