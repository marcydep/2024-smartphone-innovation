import os
import json
import time
import pandas as pd
from dotenv import load_dotenv
from openai import OpenAI

# ============================================================
# CONFIGURAZIONE
# ============================================================

load_dotenv()

client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

INPUT_FILE = "amazon_reviews_master.csv"
OUTPUT_FILE = "amazon_reviews_sentiment.csv"
CHECKPOINT_FILE = "amazon_reviews_sentiment_checkpoint.csv"

MODEL = "gpt-5.6-luna"

# ============================================================
# PROMPT DEFINITIVO
# ============================================================

SYSTEM_PROMPT = """
You are an expert annotator performing aspect-based sentiment analysis of consumer reviews of smartphones.

Analyze the review and assign a sentiment score to five dimensions:
1. Performance
2. Battery
3. Camera
4. Display
5. Overall

Evaluate ONLY these five dimensions.

The review may be written in any language. Analyze the meaning of the original text directly. Do not assume that the review is written in English or Italian.

For each dimension use exactly one of:
+2 = strongly positive
+1 = positive
0 = neutral, mixed, balanced, or genuinely ambivalent
-1 = negative
-2 = strongly negative
NA = the aspect is not mentioned or there is insufficient information to evaluate it.

IMPORTANT:
NA and 0 are different.
Use NA when the review does not provide information about the aspect.
Use 0 when the review explicitly discusses the aspect but the evaluation is neutral, mixed, balanced, or genuinely ambivalent.

ASPECT DEFINITIONS:

PERFORMANCE:
Processing speed, responsiveness, smoothness, multitasking, gaming performance, app performance, processor/chipset performance, RAM-related performance, performance stability, and overheating when explicitly connected to performance.

BATTERY:
Battery life, endurance, charging speed, charging behavior, battery drain, battery efficiency, battery health, and battery degradation.

CAMERA:
Photo quality, video quality, image processing, camera performance, low-light photography, zoom, stabilization, selfie camera, and camera-related software or features.

DISPLAY:
Screen quality, brightness, colors, contrast, resolution, refresh rate, viewing experience, outdoor visibility, touch response when specifically related to the display, and other screen characteristics.

OVERALL:
The reviewer's overall perception of the smartphone as a product.

RULES:
1. Do not infer sentiment for an aspect that is not discussed.
2. Do not use external knowledge about the smartphone, specifications, manufacturer, price, or reputation.
3. The written review is the only basis for sentiment classification.
4. If an aspect is discussed multiple times, consider the overall balance of the statements about that aspect.
5. If the reviewer compares this smartphone with another device, evaluate the sentiment expressed toward the reviewed smartphone.
6. Do not attribute opinions about another product to the reviewed smartphone.
7. A factual statement is not automatically positive or negative.
8. Consider the meaning of the review in its original language.
9. Do not provide explanations or reasoning in the output. Return only the structured JSON object.
10. Short evaluative expressions must be classified according to their explicit meaning. For example, expressions equivalent to "excellent", "very good", "good", "beautiful", or "bad" should receive the corresponding positive or negative sentiment score even when the review is very short.
11. Evaluate sentiment toward the smartphone itself, not toward Amazon, the seller, delivery, shipping, packaging, customer service, return procedures, or other aspects of the purchasing experience. Do not assign sentiment to Performance, Battery, Camera, Display, or Overall based solely on dissatisfaction with these external services or processes.
"""

SCHEMA = {
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
# LETTURA DATASET
# ============================================================

df = pd.read_csv(
    INPUT_FILE,
    keep_default_na=False
)

print(f"Recensioni totali: {len(df)}")

# ============================================================
# RIPRESA DA CHECKPOINT
# ============================================================

if os.path.exists(CHECKPOINT_FILE):

    results_df = pd.read_csv(
        CHECKPOINT_FILE,
        keep_default_na=False
    )

    completed = len(results_df)

    print(f"Checkpoint trovato: {completed} recensioni già completate.")
    print(f"Ripresa dalla recensione {completed + 1}.")

else:

    results_df = pd.DataFrame()
    completed = 0

    print("Nessun checkpoint trovato.")
    print("Inizio annotazione da zero.")

# ============================================================
# ANNOTAZIONE
# ============================================================

for i in range(completed, len(df)):

    row = df.iloc[i]

    smartphone = str(row["Smartphone"])
    review_text = str(row["review_text"])

    # Controllo testo vuoto
    if not review_text.strip():

        annotation = {
            "performance": "NA",
            "battery": "NA",
            "camera": "NA",
            "display": "NA",
            "overall": "0"
        }

    else:

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
                    "name": "sentiment_annotation",
                    "schema": SCHEMA,
                    "strict": True
                }
            }
        )

        annotation = json.loads(response.output_text)

    # Aggiunge annotazione alla riga originale
    result = row.to_dict()
    result.update(annotation)

    # Aggiunge al risultato
    results_df = pd.concat(
        [results_df, pd.DataFrame([result])],
        ignore_index=True
    )

    # Salva checkpoint DOPO ogni recensione
    results_df.to_csv(
        CHECKPOINT_FILE,
        index=False,
        encoding="utf-8-sig"
    )

    print(
        f"[{i + 1}/{len(df)}] "
        f"{smartphone} completato"
    )

    # Piccola pausa preventiva
    time.sleep(0.1)

# ============================================================
# FILE FINALE
# ============================================================

results_df.to_csv(
    OUTPUT_FILE,
    index=False,
    encoding="utf-8-sig"
)

print("\n======================================")
print("ANNOTAZIONE COMPLETATA")
print("======================================")
print(f"Recensioni annotate: {len(results_df)}")
print(f"File finale: {OUTPUT_FILE}")
print(f"Checkpoint: {CHECKPOINT_FILE}")

# ==============================================================================
# SOURCES AND AI STATEMENT
# ==============================================================================
#
# Sources:
# The code was developed by the author based on the methodological
# framework of the thesis and adapted from R documentation and
# publicly available resources where applicable.
#
# AI statement:
# Generative AI tools were used to support the development, debugging,
# and refinement of parts of the R code. The author reviewed, adapted,
# and validated the code and is responsible for the final implementation
# and results.
#
# ==============================================================================