# Efeito Escola
O trabalho realizado foi baseado em um artigo publicado em 2006 chamado [Indicador de Efeito Escola: uma metodologia para a identificação dos sucessos escolares a partir dos dados da Prova Brasil](http://td.inep.gov.br/ojs3/index.php/td/article/view/3851).
A partir de dados da Prova Brasil (hoje incorporado pelo Saeb – Sistema de Avaliação da Educação Básica) apresentava uma metodologia de avaliação de eficácia escolar baseada no quanto a escola agregou à proficiência de seus alunos, e não simplesmente na nota final obtida por ela no exame. 
Para isso, ele tem por premissa a ideia que a proficiência está associado a dois grandes fatores: os fatores escolares (qualidade do professor, da direção da escola, ambiente  escolar, etc) e os fatores extra-escolares (questões financeiras, estruturais da família e até da própria região) e são facilmente obtidas através de dados do censo e dos questionários socioeconômicos aplicados pelo próprio Saeb.
Utilizando a nota média das escolas e os dados socioeconômicos conseguimos estimar a parcela da nota que cabe aos fatores socioeconômicos e consequentemente, de forma residual, o valor agregado pelas escolas ao rendimento de seus alunos (IEE - Indicador de Efeito Escola). 
O objetivo do IEE não é servir como única de medida de avaliação da eficácia escolar, mas possibilitar a obtenção de uma amostra de escolas mais eficazes, pautada em critérios técnicos, que permita um estudo mais aprofundado de boas práticas e outras caracteríscas intraescolares.
Os scripts disponíveis referem-se aos exames do Saeb 2019 e focam apenas nas escolas públicas da rede estadual do Estado de São Paulo, mas podem ser adaptados a todo o Brasil e demais redes com suas devidas adaptações, uma vez que todos os dados são públicos.

**Fontes de Dados:**
- https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar
- https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/saeb
- https://dados.educacao.sp.gov.br/story/saresp
- http://www.imp.seade.gov.br/frontend/#/tabelas
- http://ipvs.seade.gov.br/view/index.php


Para a criação do IEE, foram realizadas as seguintes etapas do trabalho:
1.	Definição e segregação do público em sub-populações
2.	Pré-seleção qualitativa de variáveis: Como o objetivo era manter apenas as questões que refletiam os fatores extraescolares, foram excluídas quaisquer variáveis que pudessem refletir direta ou indiretamente algum fator escolar.
3.	Transformação das festures ao mesmo nível da varável modelada (escola)
5.	Seleção quantitativa de Variáveis
6.	Criação de Macro variáveis/Pilares
7.	Fitting do Modelo
8.	Criação do IEE (Indicador Efeito Escola) e Ranking das escolas


*Obs: Alguns scripts de testes, versões intermediárias do modelo e análise exploratória não foram disponibilizados*
