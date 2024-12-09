SELECT  MIN(hetio45160_0.s) 
FROM    hetio45160 AS hetio45160_0, hetio45160 AS hetio45160_1, 
        hetio45177, hetio45160 AS hetio45160_3, hetio45159 AS
        hetio45159_4, hetio45159 AS hetio45159_5 
WHERE   hetio45160_0.s = hetio45160_1.s AND hetio45160_0.d = hetio45177.s 
        AND hetio45160_1.d = hetio45177.d AND hetio45177.d = hetio45160_3.s 
        AND hetio45160_3.s = hetio45159_4.s AND hetio45160_3.d = hetio45159_5.s 
        AND hetio45159_4.d = hetio45159_5.d
