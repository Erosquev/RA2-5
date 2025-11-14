# Sistema de inventario em Haskell

- **Disciplina:** Programação Lógica e Funcional
- **Instituição:** Pontifícia Universidade Católica do Paraná    
- **Professor:** Frank Coelho de Alcantara  
- **Alunos:**  
  - Eros Felipe de Quevedo dos Santos — [@Erosquev](https://github.com/Erosquev)  
  - Victor Augusto Esmaniotto — [@Vitinnn123](https://github.com/Vitinnn123)
 
---

## Objetivo

A ideia do projeto é desenvolver um sistema de gerenciamento de inventário em Haskell, capaz de registrar, atualizar e remover itens do estoque de forma segura e totalmente persistente. O sistema também mantém um arquivo de auditoria com todas as operações executadas, permitindo identificar erros, acompanhar movimentações e gerar relatórios detalhados sobre o histórico de uso. O objetivo é proporcionar uma solução simples, funcional e confiável, que demonstre boas práticas de programação funcional, manipulação de arquivos e controle de estado.

---

## Tecnologias Utilizadas
- **Linguagem:** Haskell
- **Ambiente de execução:** OnlineGDB
- **Editor utilizado:** OnlineGDB

---

## Estrutura do Projeto

```
/Projeto Sistema de inventario em Haskell
├── 
└── README.md - Documentação do projeto
```

---

## Como Executar

1. Acesse o **OnlineGDB** (https://www.onlinegdb.com/).  
2. No editor que abrir, apague o código padrão e **cole o código do Sistema de inventario em Haskell**  
3. Em **Language**, localizada no **canto superior direito**, selecione a linguagem **Haskell**
4. Clique no botão **Run** para compilar e iniciar o programa.
5. Assim que o console abrir, você verá o menu principal do sistema.

## Exemplos de teste

1. Adicionar um item corretamente:
**Comando**
```
add P01 Teclado 10 Informatica
```
**Resposta Esperada** 
```
Item adicionado com sucesso!
```

2. Tentar adicionar item com ID duplicado:
**Comando**
```
add P01 TecladoGamer 5 Informatica
```
**Resposta Esperada** 
```
Erro: ID duplicado
```

3. Atualizar quantidade:
**Comando**
```
update P01 25
```
**Resposta Esperada** 
```
Quantidade atualizada!
```

4. Remover parcialmente:
**Comando**
```
remove P01 5
```
**Resposta Esperada** 
```
Remoção bem-sucedida!
```

5. Gerar relatório completo:
**Comando**
```
report
```
**Resposta Esperada** 
```
================== RELATÓRIO ==================
Total de registros de log: 4
----------------------------------------------
Erros registrados:
ID duplicado
----------------------------------------------
Item mais movimentado: Teclado (2 ocorrências)
==============================================
```

---

## Possíveis Melhorias Futuras

* Implementar uma interface gráfica ou painel web para substituir o uso exclusivo do terminal, facilitando o gerenciamento do inventário.
* Adicionar autenticação de usuários (admin, operador, auditor) para maior segurança no controle das operações críticas.
* Integrar o inventário com um banco de dados relacional (MySQL), garantindo maior escalabilidade e segurança.
* Criar um sistema de backup automático dos dados e logs, evitando perda de informações em caso de falha.
* Adicionar suporte a categorias hierárquicas e subcategorias, tornando a organização dos itens mais detalhada.
* Implementar filtros avançados na listagem de itens (por categoria, por quantidade, por faixa de estoque).
* Permitir exportação do inventário e dos relatórios em formatos CSV, JSON ou PDF.
* Incluir gráficos no relatório (como itens mais movimentados, itens mais críticos, histórico de alterações).
* Criar um mecanismo de logs criptografados para aumentar a integridade da auditoria.
* Adicionar suporte multilíngue (Português/Inglês) para tornar o sistema mais universal.
* Permitir integração com sistemas externos, como ERPs ou sensores IoT para atualização automática do estoque.
---

## Licença

Este projeto foi desenvolvido **exclusivamente para fins educacionais** na disciplina de *Programação Lógica e Funcional* da Pontifícia Universidade Católica do Paraná.
Não possui finalidade comercial e não concede direitos de uso além do contexto acadêmico.
