import json
import pandas as pd
from openai import OpenAI
from dotenv import load_dotenv

# ============================================================
# 1. CONFIGURAZIONE
# ============================================================

load_dotenv()
client = OpenAI()

MODEL = "gpt-5.6-luna"

# ============================================================
# 2. PROMPT
# ============================================================

SYSTEM_PROMPT = """
You are an expert annotator performing aspect-based sentiment
analysis of consumer reviews of smartphones.

Analyze the review and assign a sentiment score to five dimensions:

1. Performance
2. Battery
3. Camera
4. Display
5. Overall

Evaluate ONLY these five dimensions.

The review may be written in any language. Analyze the meaning
of the original text directly. Do not assume that the review is
written in English or Italian.

For each dimension use exactly one of:

+2 = strongly positive
+1 = positive
 0 = neutral, mixed, balanced, or genuinely ambivalent
-1 = negative
-2 = strongly negative
NA = the aspect is not mentioned or there is insufficient
     information to evaluate it.

IMPORTANT:
NA and 0 are different.

Use NA when the review does not provide information about
the aspect.

Use 0 when the review explicitly discusses the aspect but
the evaluation is neutral, mixed, balanced, or genuinely
ambivalent.

ASPECT DEFINITIONS:

PERFORMANCE:
Processing speed, responsiveness, smoothness, multitasking,
gaming performance, app performance, processor/chipset
performance, RAM-related performance, performance stability,
and overheating when explicitly connected to performance.

BATTERY:
Battery life, endurance, charging speed, charging behavior,
battery drain, battery efficiency, battery health, and
battery degradation.

CAMERA:
Photo quality, video quality, image processing, camera
performance, low-light photography, zoom, stabilization,
selfie camera, and camera-related software or features.

DISPLAY:
Screen quality, brightness, colors, contrast, resolution,
refresh rate, viewing experience, outdoor visibility,
touch response when specifically related to the display,
and other screen characteristics.

OVERALL:
The reviewer's overall perception of the smartphone as a product.

RULES:

1. Do not infer sentiment for an aspect that is not discussed.

2. Do not use external knowledge about the smartphone,
   specifications, manufacturer, price, or reputation.

3. The written review has priority over the numerical
   Amazon star rating. Do not use the star rating to infer
   sentiment for an aspect that is not discussed.

4. If an aspect is discussed multiple times, consider the
   overall balance of the statements about that aspect.

5. If the reviewer compares this smartphone with another
   device, evaluate the sentiment expressed toward the
   reviewed smartphone.

6. Do not attribute opinions about another product to the
   reviewed smartphone.

7. A factual statement is not automatically positive or
   negative. For example, stating a camera resolution without
   evaluating it does not constitute positive sentiment.

8. Consider the meaning of the review in its original language.
   Reviews can be written in Italian, English, French, German,
   Spanish, or other languages.

9. Do not provide explanations or reasoning in the output.
   Return only the structured JSON object.
"""

# ============================================================
# 3. JSON SCHEMA
# ============================================================

SENTIMENT_SCHEMA = {
    "type": "object",
    "properties": {
        "performance": {
            "type": "string",
            "enum": ["-2", "-1", "0", "1", "2", "NA"]
        },
        "battery": {
            "type": "string",
            "enum": ["-2", "-1", "0", "1", "2", "NA"]
        },
        "camera": {
            "type": "string",
            "enum": ["-2", "-1", "0", "1", "2", "NA"]
        },
        "display": {
            "type": "string",
            "enum": ["-2", "-1", "0", "1", "2", "NA"]
        },
        "overall": {
            "type": "string",
            "enum": ["-2", "-1", "0", "1", "2"]
        }
    },
    "required": [
        "performance",
        "battery",
        "camera",
        "display",
        "overall"
    ],
    "additionalProperties": False
}

# ============================================================
# 4. LETTURA DEL MASTER
# ============================================================

df = pd.read_csv("amazon_reviews_master.csv")

# Prendiamo una recensione reale del dataset.
review = df.iloc[0]

smartphone = review["Smartphone"]
review_text = review["review_text"]
rating = review.get("rating", "N/A")

# ============================================================
# 5. CHIAMATA API
# ============================================================

response = client.responses.create(
    model=MODEL,

    input=[
        {
            "role": "system",
            "content": SYSTEM_PROMPT
        },
        {
            "role": "user",
            "content": f"""
Analyze the following smartphone review.

Smartphone:
{smartphone}

Amazon rating:
{rating}

Review:
\"\"\"
{review_text}
\"\"\"
"""
        }
    ],

    text={
        "format": {
            "type": "json_schema",
            "name": "smartphone_sentiment",
            "schema": SENTIMENT_SCHEMA,
            "strict": True
        }
    }
)

# ============================================================
# 6. RISULTATO
# ============================================================

result = json.loads(response.output_text)

print("\n" + "=" * 70)
print("SMARTPHONE")
print("=" * 70)
print(smartphone)

print("\n" + "=" * 70)
print("RECENSIONE")
print("=" * 70)
print(review_text)

print("\n" + "=" * 70)
print("SENTIMENT")
print("=" * 70)
print(json.dumps(result, indent=2, ensure_ascii=False))