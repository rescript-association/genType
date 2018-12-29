[@genType]
 type fact = {
   id: string,
   subject: string,
 };
 
 [@genType]
 let out = (): fact => {id: "test1", subject: "test2"};
 
 [@genType]
 let inn = (t: fact) => t.id;
 