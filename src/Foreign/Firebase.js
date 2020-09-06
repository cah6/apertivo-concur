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

exports.signInWithRedirectImpl = function () {
  var provider = new firebase.auth.GoogleAuthProvider();

  firebase.auth().signInWithRedirect(provider);
  return firebase.auth().getRedirectResult();
}

exports.signInWithPopupImpl = function () {
  var provider = new firebase.auth.GoogleAuthProvider();

  return firebase.auth().signInWithPopup(provider);
}
