# 1. Cargar librerías
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
# Busqueda hiperparametros
from sklearn.model_selection import train_test_split 
from interpret.glassbox import ExplainableBoostingClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import roc_auc_score, make_scorer
# Resultados
from interpret import show
# Advertencia AUC
from sklearn.metrics import roc_auc_score
def custom_auc_score(estimator, X, y):
    probas = estimator.predict_proba(X)[:, 1]
    return roc_auc_score(y, probas)

# 2. Leer datos
# Ruta de los archivos (ajústala según tu sistema)
train_path = "C:/Users/edama/Desktop/Ucr_2025/Analisis_Datos/Bitacora_3/train.csv"
test_path  = "C:/Users/edama/Desktop/Ucr_2025/Analisis_Datos/Bitácora_1/test.csv"

# Leer archivos CSV
datos = pd.read_csv(train_path)
TEST = pd.read_csv(test_path)

# 3. Limpiar datos (eliminar filas con valores faltantes)
datos = datos.dropna()
TEST_MOD = TEST[[
    "Customer Type", "Inflight wifi service", "Departure/Arrival time convenient",
    "Ease of Online booking", "Gate location", "Food and drink", "Online boarding", "Seat comfort",
    "Inflight entertainment", "On-board service", "Leg room service", "Baggage handling",
    "Checkin service", "Inflight service", "Cleanliness", "Departure Delay in Minutes",
    "Arrival Delay in Minutes", "satisfaction"
]].dropna()

# 4. Codificar la variable respuesta como binaria
datos["satisfaction_bin"] = (datos["satisfaction"] == "satisfied").astype(int)
TEST_MOD["satisfaction_bin"] = (TEST_MOD["satisfaction"] == "satisfied").astype(int)

# 5. Convertir variables categóricas a tipo 'category'
variables_categoricas = [
    "Customer Type", "Inflight wifi service", "Departure/Arrival time convenient",
    "Ease of Online booking", "Gate location", "Food and drink", "Online boarding", "Seat comfort",
    "Inflight entertainment", "On-board service", "Leg room service", "Baggage handling",
    "Checkin service", "Inflight service", "Cleanliness"
]

for var in variables_categoricas:
    datos[var] = datos[var].astype("category")
    TEST_MOD[var] = TEST_MOD[var].astype("category")

# 6. Variables predictoras seleccionadas
predictoras = [
    "Customer Type", "Inflight wifi service", "Departure/Arrival time convenient",
    "Ease of Online booking", "Gate location", "Food and drink", "Online boarding", "Seat comfort",
    "Inflight entertainment", "On-board service", "Leg room service", "Baggage handling",
    "Checkin service", "Inflight service", "Cleanliness", "Departure Delay in Minutes",
    "Arrival Delay in Minutes"
]

# 7. Crear dataset final para modelar
X = datos[predictoras]
y = datos["satisfaction_bin"]

X_test_mod = TEST_MOD[predictoras]
y_test_mod = TEST_MOD["satisfaction_bin"]

# Partir dataset de entrenamiento en entrenamiento y validación
X_train, X_val, y_train, y_val = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y)

# 8. Entrenamiento y búsqueda de hiperparámetros con GridSearchCV
ebm = ExplainableBoostingClassifier(random_state=42)

param_grid = {
    "interactions": [0, 10],
    "learning_rate": [0.01, 0.05],
    "max_bins": [128, 256],
    "max_leaves": [2, 3],
    "max_rounds": [100, 200]  # <--- Este es el correcto
}

auc_scorer = make_scorer(custom_auc_score, greater_is_better=True)

grid_search = GridSearchCV(
    estimator=ebm,
    param_grid=param_grid,
    scoring=custom_auc_score,  # Arreglar advertencia  (sin make_scorer)
    cv=3,
    n_jobs=-1,
    verbose=1
)

grid_search.fit(X_train, y_train)
print("Mejores hiperparámetros:", grid_search.best_params_)
print("Mejor AUC en validación:", grid_search.best_score_)

# 9. Evaluación en el conjunto TEST
y_prob_test = grid_search.best_estimator_.predict_proba(X_test_mod)[:, 1]
y_pred_test = grid_search.best_estimator_.predict(X_test_mod)

from sklearn.metrics import roc_auc_score, confusion_matrix

print("AUC en test:", roc_auc_score(y_test_mod, y_prob_test))
print("Matriz de confusión en test:\n", confusion_matrix(y_test_mod, y_pred_test))

# 10, Visualizacion
exp = grid_search.best_estimator_.explain_global()
show(exp)

# 11. Importancia de variables según EBM
# Obtener nombres e importancias (¡con paréntesis!)
nombres_terminos = grid_search.best_estimator_.term_names_
importancias = grid_search.best_estimator_.term_importances()

# Crear y ordenar el DataFrame
importancia_variables = pd.DataFrame({
    'Término': nombres_terminos,
    'Importancia': importancias
}).sort_values(by='Importancia', ascending=False)

# Mostrar top 17
print("\nTop términos más influyentes según el modelo EBM:")
print(importancia_variables.head(40))


