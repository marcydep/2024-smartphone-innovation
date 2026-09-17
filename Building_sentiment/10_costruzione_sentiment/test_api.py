from openai import OpenAI
from dotenv import load_dotenv

load_dotenv()

client = OpenAI()

response = client.responses.create(
    model="gpt-5.6-luna",
    input="Rispondi solamente con: API FUNZIONANTE"
)

print(response.output_text)