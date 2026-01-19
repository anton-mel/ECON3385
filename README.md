# Problem Set: How to Run

`<anton.melnychuk@yale.edu>`

Find all the solutions committed in the GitHub repo:
https://github.com/anton-mel/ECON3385. Navigate to the branch `<psetID>` for respective problem set.

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
