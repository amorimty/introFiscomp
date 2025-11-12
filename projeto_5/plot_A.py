import matplotlib.pyplot as plt


# Função para ler duas colunas numéricas de um arquivo
def ler_dados(arquivo):
    x, y = [], []
    with open(arquivo, "r") as f:
        for linha in f:
            if linha.strip():  # Ignora linhas vazias
                partes = linha.split()
                if len(partes) >= 2:
                    x.append(float(partes[0]))
                    y.append(float(partes[1]))
    return x, y


# Nomes dos arquivos
arquivos = [
    "/home/amorimty/Desktop/programming/introFiscomp/introFiscomp/projeto_5/map.dat"
]

# Nomes para legenda (você pode mudar aqui)
nomes = ["x"]

# Ler os dados dos três arquivos
trajetorias = [ler_dados(arquivo) for arquivo in arquivos]

# Cores das trajetórias
cores = ["red"]


# Plotar
plt.figure(figsize=(8, 6))
for (x, y), cor, nome in zip(trajetorias, cores, nomes):
    plt.plot(x, y, color=cor, label=nome, linewidth=1.8)

plt.xlabel("Eixo X")
plt.ylabel("Eixo Y")
plt.title("Trajetórias")
plt.legend()
plt.grid(True)
plt.tight_layout()
plt.show()
