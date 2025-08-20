# 📖 MSc Thesis – Kolmogorov–Arnold Networks vs Multilayer Perceptrons

This repository contains material from my **Master’s Thesis in Economics (Data Science track)**,  
University of Naples Federico II (2024).  

The thesis presents one of the first **systematic comparisons** between **Kolmogorov–Arnold Networks (KANs)** and traditional **Multilayer Perceptrons (MLPs)**, highlighting how **KANs achieve superior performance** in terms of accuracy, robustness, and interpretability across a variety of datasets.

---

## 🔎 Introduction
Deep Learning has revolutionized artificial intelligence, but traditional **MLPs** show important limitations:  
- high number of parameters,  
- limited interpretability,  
- overfitting risk with small datasets,  
- rising computational costs.  

**Kolmogorov–Arnold Networks (KANs)**, inspired by the Kolmogorov–Arnold representation theorem, introduce an innovative architecture capable of modeling multivariate functions through univariate functional approximations.  
Thanks to B-spline parameterization, grid extension, and pruning techniques, KANs combine **parsimony, transparency, and predictive strength**, overcoming many weaknesses of MLPs.  

---

## 🎯 Research objectives
- Compare **KANs vs. MLPs** on multiple synthetic and real datasets.  
- Test whether KANs outperform MLPs not only in **interpretability**, but also in **accuracy and generalization**.  
- Assess their potential in fields where **explainability and predictive reliability** are critical.  

---

## 🛠️ Methodology
- **Models studied**:  
  - *MLPs*: standard deep feedforward networks.  
  - *KANs*: networks leveraging B-spline based layers with advanced features (grid extension, sparsification, pruning, symbolification).  

- **Tasks**: regression and classification.  
- **Datasets**: synthetic datasets (function approximation) + real-world clinical and economic datasets.  
- **Metrics**: accuracy, MSE, robustness to overfitting, parameter count, computational efficiency.  
- **Tools**: Python, PyTorch, Scikit-learn.  

---

## 📊 Experimental results
- **KANs consistently outperformed MLPs in predictive accuracy**, especially on small and medium-sized datasets.  
- They proved **more robust to overfitting**, generalizing better when data were limited.  
- **Interpretability**: KANs generated symbolic functional forms, enabling understanding of variable relationships.  
- **Parsimony**: fewer parameters required compared to MLPs, reducing model complexity.  
- **MLPs** remained advantageous only in terms of training speed on very large datasets.  

Overall, **KANs demonstrated themselves as a superior alternative** to MLPs in contexts where accuracy, robustness, and interpretability are simultaneously required.  

---

## 🧩 Key contributions
- Provided one of the **first empirical validations** of KANs against MLPs.  
- Demonstrated **higher accuracy and robustness** of KANs across diverse tasks.  
- Showed how KANs reduce overfitting and enable continual learning.  
- Highlighted KANs as a **cutting-edge architecture** with strong potential for domains such as medicine, finance, and policy analysis.  

---

## 📂 Repository content
- `summary.pdf` → thesis defense slides.  
- `code_demo.ipynb` → demo implementation (KAN vs MLP on a synthetic dataset).  
- `README.md` → detailed description of the thesis.  

---

## 🔗 References
- Kolmogorov, A. N. (1957). *On the representation of continuous functions of several variables by superpositions of continuous functions of one variable and addition*.  
- He, Y., et al. (2023). *Kolmogorov–Arnold Networks*. arXiv:2309.04137.  
- Original Thesis: *Kolmogorov–Arnold Networks vs Multilayer Perceptrons* (2024), University of Naples Federico II.  

