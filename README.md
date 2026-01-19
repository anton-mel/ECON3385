# Problem Set 1: How to Run

`<anton.melnychuk@yale.edu>`

## Setup

1. Create and activate virtual environment:
```bash
python3 -m venv venv
source venv/bin/activate  # On macOS/Linux
```

2. Install dependencies:
```bash
pip install -r requirements.txt
pip install jupyter ipykernel
```

## Run Notebook

```bash
source venv/bin/activate
jupyter notebook ps1_solution.ipynb
```

Or execute directly:
```bash
source venv/bin/activate
jupyter nbconvert --to notebook --execute --inplace ps1_solution.ipynb
```
