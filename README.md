# Belesminha

**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01 <br>
**Nro do Grupo (de acordo com a Planilha de Divisão dos Grupos)**: 02<br>
**Paradigma**: Funcional<br>

## Alunos
|Matrícula | Aluno |
| -- | -- |
|Ailton Aires|180011600|
|Arthur Sena|180030345|
|Eric Chagas de Oliveira|180119508|
|Fernando Vargas|180016491|
|Gabriel Luiz|190013354|
|Guilherme Daniel Fernandes da Silva|180018019|
|Kevin Luis|180042386|
|Matheus Monteiro|180127969|
|Thiago Vivan Bastos|190020407|
|Victor Buendia|190020601|

## Sobre 
Descreva o seu projeto em linhas gerais. 
Use referências, links, que permitam conhecer um pouco mais sobre o projeto.
Capriche nessa seção, pois ela é a primeira a ser lida pelos interessados no projeto.

## Screenshots
Adicione 2 ou mais screenshots do projeto em termos de interface e/ou funcionamento.

## Instalação 
**Linguagens**: Haskell<br>
**Tecnologias**: Gloss<br>

Pré requisitos:

- Makefile
- Docker
- Docker Compose

Descreva os pré-requisitos para rodar o seu projeto e os comandos necessários.
Insira um manual ou um script para auxiliar ainda mais.
Gifs animados e outras ilustrações são bem-vindos!

## Uso

O uso do projeto está facilitado pelos autores utilizando `Makefiles` e a instalação funciona para ambientes Linux ou MacOS. Também se pode *buildar* o projeto usando Docker. 

### Docker (Recomendado)

Você deve estar em um Linux para este método funcionar, porque o Docker compila a imagem dentro de um Ubuntu e não é possível rodar uma imagem de MacOS. Basta apenas executar o comando abaixo.

```sh
make docker-build
```

### Linux

Basta apenas executar o comando abaixo. Contudo, para a instalação do Haskell no ambiente Linux, será necessário confirmar todas as solicitudes do GHCup pressionando sempre ENTER.

```sh
make install && make
```

### MacOS

Basta apenas executar o comando abaixo.

```sh
make install && sudo make
```

## Vídeo
Adicione 1 ou mais vídeos com a execução do projeto.
Procure: 
(i) Introduzir o projeto;
(ii) Mostrar passo a passo o código, explicando-o, e deixando claro o que é de terceiros, e o que é contribuição real da equipe;
(iii) Apresentar particularidades do Paradigma, da Linguagem, e das Tecnologias, e
(iV) Apresentar lições aprendidas, contribuições, pendências, e ideias para trabalhos futuros.
OBS: TODOS DEVEM PARTICIPAR, CONFERINDO PONTOS DE VISTA.
TEMPO: +/- 15min

## Participações
Apresente, brevemente, como cada membro do grupo contribuiu para o projeto.
|Nome do Membro | Contribuição | Significância da Contribuição para o Projeto (Excelente/Boa/Regular/Ruim/Nula) |
| -- | -- | -- |
|Ailton Aires|||
|Arthur Sena| - Randomização da Adição de Folhas no Mapa <br/> - Impressão das folhas no mapa <br/> - Função de restart do jogo através de evento do teclado |Boa|
|Eric Chagas de Oliveira|||
|Fernando Vargas|- Criação do mapa fixo do labirinto <br/> - Tentativa de criar um DFS para gerar o labirinto (ideia abandonada) <br/> - Refatorações de funções com o objetivo de seguir as convenções da comunidade do Haskell| Boa |
|Gabriel Luiz|- Implementação da validação que impede o usuário de ultrapassar paredes <br/> - Implementação do contador de folhas comidas <br/> - Implementação da funcionalidade de reiniciar o jogo (finalizada pelo Victor)| Boa
|Guilherme Daniel Fernandes da Silva|||
|Kevin Luis|- Criação do mapa fixo do labirinto <br/> - Implementação da detecção de eventos do teclado (Primeira versão) <br/> - Algoritmos de geração de folhas e Impressão de folhas no mapa<br/> - Tentativa de implementar ranking com leitura e escrita de arquivo (as funcões estão prontas e funcionando na branch `feat/history`, porém ocorreu um problema na integração com o gloss)|Boa|
|Matheus Monteiro|||
|Thiago Vivan Bastos|||
|Victor Buendia|- Criação dos tipos de dados para labirinto <br/> - Criação do mapa fixo do labirinto <br/> - Cria função de checagem se ponto faz parte do limite do labirinto <br/> - Criação do tipo de dados e dinâmica de direção no labirinto <br/> - Tentativa de criar um DFS para gerar o labirinto (ideia abandonada) <br/> - Exibição do caminho ideal na tela <br/> - Contador de passos do usuário e impressão na tela <br/> - Impressão do caminho/rastro do usuário à medida que se move <br/> - Ajuda na implementação da movimentação do player <br/> - Implementação do Gloss no projeto <br/> - Criação da Makefile para instalação e compilação do projeto <br/> Reset do jogo |Excelente|

## Outros 
Quaisquer outras informações sobre o projeto podem ser descritas aqui. Não esqueça, entretanto, de informar sobre:
(i) Lições Aprendidas;
(ii) Percepções;
(iii) Contribuições e Fragilidades, e
(iV) Trabalhos Futuros.

## Fontes
Inspiração:
> https://github.com/UnBParadigmas2023-2/2023.2_G2_Funcional_EscapeFromCurry/tree/main
> https://homepages.dcc.ufmg.br/~flavioro/belesminha/Artigo-Belesminha.pdf
