exports.getLocationPromise = () => {
  return new Promise((resolve, reject) => {
    navigator.geolocation.getCurrentPosition((response, err) => {
      if (err) return reject(err)
      resolve(response.coords)
    })
  })
}
