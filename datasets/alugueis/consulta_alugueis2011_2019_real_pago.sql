select	a11.ID_ANO_LANC  ID_ANO,
	a11.ID_ANO_LANC  ID_ANO_LANC,
	a11.ID_MES_LANC  ID_MES_LANC,
	max(a125.SG_MES_COMPLETO)  SG_MES_COMPLETO,
	a11.ID_ANO_LANC  ID_ANO0,
	a11.ID_MES_LANC  ID_MES,
	a11.ID_DIA_LANC  ID_DIA,
	max(a114.NO_DIA_COMPLETO)  NO_DIA_COMPLETO,
	a17.ID_ORGAO_MAXI  ID_ORGAO_MAXI,
	max(a119.CO_ORGAO_MAXI)  CO_ORGAO_MAXI,
	max(a119.NO_ORGAO_MAXI)  NO_ORGAO_MAXI,
	a15.ID_ORGAO_UG  ID_ORGAO,
	max(a112.CO_ORGAO)  CO_ORGAO,
	max(a112.NO_ORGAO)  NO_ORGAO,
	a15.ID_UG  ID_UG_EXEC,
	max(a111.CO_UG)  CO_UG,
	max(a111.NO_UG)  NO_UG,
	a11.ID_FUNCAO_PT  ID_FUNCAO_PT,
	max(a115.NO_FUNCAO_PT)  ID_FUNCAO_PT0,
	a11.ID_SUBFUNCAO_PT  ID_SUBFUNCAO_PT,
	max(a118.NO_SUBFUNCAO_PT)  NO_SUBFUNCAO_PT,
	a11.ID_PROGRAMA_PT  ID_PROGRAMA_PT,
	max(a117.NO_PROGRAMA_PT)  NO_PROGRAMA_PT,
	a11.ID_ACAO_PT  ID_ACAO_PT,
	max(a113.NO_ACAO_PT)  NO_ACAO_PT,
	a11.ID_GRUPO_DESPESA_NADE  ID_GRUPO_DESPESA_NADE,
	max(a123.NO_GRUPO_DESPESA_NADE)  NO_GRUPO_DESPESA_NADE,
	a11.ID_ELEMENTO_DESPESA_NADE  ID_ELEMENTO_DESPESA_NADE,
	max(a120.CO_ELEMENTO_DESPESA_NADE)  CO_NATUREZA_DESPESA,
	max(a120.NO_ELEMENTO_DESPESA_NADE)  NO_NATUREZA_DESPESA,
	a11.ID_CATEGORIA_ECONOMICA_NADE  ID_CATEGORIA_ECONOMICA_NADE,
	a11.ID_GRUPO_DESPESA_NADE  ID_GRUPO_DESPESA_NADE0,
	a11.ID_MOAP_NADE  ID_MOAP_NADE,
	a11.ID_ELEMENTO_DESPESA_NADE  ID_ELEMENTO_DESPESA_NADE0,
	a11.ID_SUBITEM_NADE  ID_SUBITEM_NADE,
	max(a116.CO_NATUREZA_DESPESA_DETA)  CO_NATUREZA_DESPESA_DETA,
	max(a116.NO_NATUREZA_DESPESA_DETA)  NO_NATUREZA_DESPESA_DETA,
	a11.ID_DOCUMENTO_CCOR  ID_DOCUMENTO,
	a19.ID_TP_ENTIDADE_FAVO_DOC  ID_TP_ENTIDADE,
	a19.ID_ENTIDADE_FAVO_DOC  ID_ENTIDADE,
	max(a122.NO_ENTIDADE)  NO_ENTIDADE,
	a11.ID_DOCUMENTO_LANC  ID_DOCUMENTO0,
	a11.ID_ANO_LANC  ID_ANO_EMISSAO_DOC,
	a110.ID_ESPECIE_NE  ID_ESPECIE_NE,
	max(a126.NO_ESPECIE_NE)  NO_ESPECIE_NE,
	a18.TX_OBSERVACAO  TX_OBSERVACAO,
	a11.ID_TP_ENTIDADE_FAVO_DOC  ID_TP_ENTIDADE0,
	a11.ID_ENTIDADE_FAVO_DOC  ID_ENTIDADE0,
	max(a121.NO_ENTIDADE)  NO_ENTIDADE0,
	a14.ID_ITEM_INFORMACAO  ID_ITEM_INFORMACAO,
	max(a124.NO_ITEM_INFORMACAO)  NO_ITEM_INFORMACAO,
	max(a124.CO_ITEM_INFORMACAO)  CO_ITEM_INFORMACAO,
	sum(((a11.VA_MOVIMENTO_LIQUIDO * a14.IN_OPERACAO_EXPRESSAO) * a13.PE_TAXA))  SALDORITEMINFORMAODIALANAMENT
from	WF_LANCAMENTO	a11
	join	WD_MOEDA	a12
	  on 	(a11.ID_MOEDA_UG_EXEC_H = a12.ID_MOEDA)
	join	WD_TAXA_CAMBIO_MENSAL	a13
	  on 	(a12.ID_MOEDA = a13.ID_MOEDA_ORIGEM)
	join	WD_ITEM_DECODIFICADO_CCON	a14
	  on 	(a11.ID_ANO_LANC = a14.ID_ANO_ITEM_CONTA and 
	a11.ID_CONTA_CONTABIL_LANC = a14.ID_CONTA_CONTABIL)
	join	WD_UG_EXERCICIO	a15
	  on 	(a11.ID_ANO_LANC = a15.ID_ANO and 
	a11.ID_UG_EXEC = a15.ID_UG)
	join	WD_ORGAO_EXERCICIO	a16
	  on 	(a15.ID_ANO = a16.ID_ANO and 
	a15.ID_ORGAO_UG = a16.ID_ORGAO)
	join	WD_ORGAO_SUPE_EXERCICIO	a17
	  on 	(a16.ID_ANO = a17.ID_ANO and 
	a16.ID_ORGAO_SUPE = a17.ID_ORGAO_SUPE)
	join	WD_DOCUMENTO	a18
	  on 	(a11.ID_ANO_LANC = a18.ID_ANO_EMISSAO_DOC and 
	a11.ID_DOCUMENTO_LANC = a18.ID_DOCUMENTO)
	join	WD_DOC_NE	a19
	  on 	(a11.ID_DOCUMENTO_CCOR = a19.ID_DOC_NE)
	join	WD_DOC_NE	a110
	  on 	(a18.ID_DOC_NE = a110.ID_DOC_NE)
	join	WD_UG	a111
	  on 	(a15.ID_UG = a111.ID_UG)
	join	WD_ORGAO	a112
	  on 	(a15.ID_ORGAO_UG = a112.ID_ORGAO)
	join	WD_ACAO_PT	a113
	  on 	(a11.ID_ACAO_PT = a113.ID_ACAO_PT)
	join	WD_DIA	a114
	  on 	(a11.ID_ANO_LANC = a114.ID_ANO and 
	a11.ID_DIA_LANC = a114.ID_DIA and 
	a11.ID_MES_LANC = a114.ID_MES)
	join	WD_FUNCAO_PT	a115
	  on 	(a11.ID_FUNCAO_PT = a115.ID_FUNCAO_PT)
	join	WD_NATUREZA_DESPESA_DETA	a116
	  on 	(a11.ID_CATEGORIA_ECONOMICA_NADE = a116.ID_CATEGORIA_ECONOMICA_NADE and 
	a11.ID_ELEMENTO_DESPESA_NADE = a116.ID_ELEMENTO_DESPESA_NADE and 
	a11.ID_GRUPO_DESPESA_NADE = a116.ID_GRUPO_DESPESA_NADE and 
	a11.ID_MOAP_NADE = a116.ID_MOAP_NADE and 
	a11.ID_SUBITEM_NADE = a116.ID_SUBITEM_NADE)
	join	WD_PROGRAMA_PT	a117
	  on 	(a11.ID_PROGRAMA_PT = a117.ID_PROGRAMA_PT)
	join	WD_SUBFUNCAO_PT	a118
	  on 	(a11.ID_SUBFUNCAO_PT = a118.ID_SUBFUNCAO_PT)
	join	WD_ORGAO_MAXI	a119
	  on 	(a17.ID_ORGAO_MAXI = a119.ID_ORGAO_MAXI)
	join	WD_ELEMENTO_DESPESA_NADE	a120
	  on 	(a11.ID_ELEMENTO_DESPESA_NADE = a120.ID_ELEMENTO_DESPESA_NADE)
	join	WD_ENTIDADE	a121
	  on 	(a11.ID_ENTIDADE_FAVO_DOC = a121.ID_ENTIDADE and 
	a11.ID_TP_ENTIDADE_FAVO_DOC = a121.ID_TP_ENTIDADE)
	join	WD_ENTIDADE	a122
	  on 	(a19.ID_ENTIDADE_FAVO_DOC = a122.ID_ENTIDADE and 
	a19.ID_TP_ENTIDADE_FAVO_DOC = a122.ID_TP_ENTIDADE)
	join	WD_GRUPO_DESPESA_NADE	a123
	  on 	(a11.ID_GRUPO_DESPESA_NADE = a123.ID_GRUPO_DESPESA_NADE)
	join	WD_ITEM_INFORMACAO	a124
	  on 	(a14.ID_ITEM_INFORMACAO = a124.ID_ITEM_INFORMACAO)
	join	WD_MES	a125
	  on 	(a11.ID_ANO_LANC = a125.ID_ANO and 
	a11.ID_MES_LANC = a125.ID_MES)
	join	WD_ESPECIE_NE	a126
	  on 	(a110.ID_ESPECIE_NE = a126.ID_ESPECIE_NE)
where	(a14.ID_ITEM_INFORMACAO in (61)
 and a112.ID_ORCA_FISCAL_ORGAO in (0)
 and a116.CO_NATUREZA_DESPESA_DETA in ('33903615', '33903910', '33913910', '44903615', '44903910')
 and a11.ID_ANO_LANC >=  2012
 and a111.ID_MOEDA_UG in (790)
 and a13.ID_ANO = a11.ID_ANO_LANC
 and a13.ID_MES = a11.ID_MES_LANC)
group by	a11.ID_ANO_LANC,
	a11.ID_ANO_LANC,
	a11.ID_MES_LANC,
	a11.ID_ANO_LANC,
	a11.ID_MES_LANC,
	a11.ID_DIA_LANC,
	a17.ID_ORGAO_MAXI,
	a15.ID_ORGAO_UG,
	a15.ID_UG,
	a111.ID_MOEDA_UG,
	a11.ID_FUNCAO_PT,
	a11.ID_SUBFUNCAO_PT,
	a11.ID_PROGRAMA_PT,
	a11.ID_ACAO_PT,
	a11.ID_GRUPO_DESPESA_NADE,
	a11.ID_ELEMENTO_DESPESA_NADE,
	a11.ID_CATEGORIA_ECONOMICA_NADE,
	a11.ID_GRUPO_DESPESA_NADE,
	a11.ID_MOAP_NADE,
	a11.ID_ELEMENTO_DESPESA_NADE,
	a11.ID_SUBITEM_NADE,
	a11.ID_DOCUMENTO_CCOR,
	a19.ID_TP_ENTIDADE_FAVO_DOC,
	a19.ID_ENTIDADE_FAVO_DOC,
	a11.ID_DOCUMENTO_LANC,
	a11.ID_ANO_LANC,
	a110.ID_ESPECIE_NE,
	a18.TX_OBSERVACAO,
	a11.ID_TP_ENTIDADE_FAVO_DOC,
	a11.ID_ENTIDADE_FAVO_DOC,
	a14.ID_ITEM_INFORMACAO