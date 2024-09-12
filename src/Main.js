const urlParams = new URLSearchParams(window.location.search);

export const getName = () => {
  const n = urlParams.get('q');
  return n === null ? "" : n
}

export const getRandomSubset = (k) => (array) => {
  const n = array.length;
  if (k > n) {
    throw new Error("k cannot be greater than n");
  }

  const result = [];
  for (let i = 0; i < k; i++) {
    const randomIndex = Math.floor(Math.random() * (n - i)) + i;
    [array[i], array[randomIndex]] = [array[randomIndex], array[i]];
    result.push(array[i]);
  }

  shuffleInPlace(result);
  return result;
}

function shuffleInPlace(array) {
  const n = array.length;

  for (let i = n - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
}