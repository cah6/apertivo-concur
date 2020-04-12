exports.getCollectionImpl = function () {
  return firebase.firestore().collection("happy-hours")
        .get()
        .then(querySnapshot => {
          let results = querySnapshot.docs.map(doc => {
            retData = doc.data()
            retData["id"] = doc.id;
            return retData;
          });
          return results;
        })
}
