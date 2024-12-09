SELECT MIN(hetio45173_0.s)
FROM   hetio45173 AS hetio45173_0, hetio45173 AS hetio45173_1, 
       hetio45160 AS hetio45160_2, hetio45160 AS hetio45160_3, 
       hetio45160 AS hetio45160_4, hetio45159 AS hetio45159_5, 
       hetio45159 AS hetio45159_6 
WHERE  hetio45173_0.s = hetio45173_1.s AND hetio45173_0.d = hetio45160_2.s AND 
       hetio45173_1.d = hetio45160_3.s AND hetio45160_2.d = hetio45160_3.d AND 
       hetio45160_3.d = hetio45160_4.s AND hetio45160_4.s = hetio45159_5.s AND 
       hetio45160_4.d = hetio45159_6.s AND hetio45159_5.d = hetio45159_6.d
