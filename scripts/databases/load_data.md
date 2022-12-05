# Load data into databases

---

## Create and activate virtual environment

* Windows
  
```bat
python -m venv librarink_venv && librarink_venv\scripts\activate.bat 
```

* Linux

```bash
apt install python3-venv
apt install default-libmysqlclient-dev
python3 -m venv librarink_venv && source librarink_venv/bin/activate
```

---

## Install requirements

```bash
python -m pip install -r requirements.txt
```

---

### Launch script

```bash
python load_data.py
```

---
