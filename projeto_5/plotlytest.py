import pandas as pd
import plotly.express as px
from plotly.offline import iplot
import plotly.graph_objs as go


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


x, y = ler_dados(
    r"D:/programação no windows/introFiscomp/introFiscomp/projeto_5/map_pontos.dat"
)
df1 = pd.DataFrame({"xi": x, "xi+1": y})

z, h = ler_dados(
    r"D:/programação no windows/introFiscomp/introFiscomp/projeto_5/map_linha.dat"
)
df2 = pd.DataFrame({"xi": z, "xi+1": h})


trace1 = go.Scatter(
    x=df1["xi"],
    y=df1["xi+1"],
    mode="markers",
    name="pontos",
    line=dict(color="#0f26f5"),
)
trace2 = go.Scatter(
    x=df2["xi"],
    y=df2["xi+1"],
    mode="lines",
    name="linha",
    line=dict(color="#f5160f"),
)

layout = go.Layout(
    title="Mapa Logístico",
    xaxis=dict(title="xi"),
    yaxis=dict(title="xi+1"),
    shapes=[
        {
            "type": "line",
            "x0": 0,
            "y0": 0,
            "x1": 1,
            "y1": 1,
            "line": {"color": "rgb(50, 171, 96)", "width": 4},
        }
    ],
)
fig = go.Figure(layout=layout, data=[trace1, trace2])


# df = pd.read_csv("projeto_5/map.dat", delimiter="\t", header=None)


# fig = px.scatter(x="xi", y="xi+1", data_frame=df, title="Mapa Logístico")

# fig.write_image("projeto_5/mapa_logistico.svg")

fig.show()
