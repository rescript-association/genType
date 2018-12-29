open Test1;
 
 [@genType]
 let out = (): fact => {id: "test1", subject: "test2"};
 
 [@genType]
 let inn = (t: fact) => t.id; 
 