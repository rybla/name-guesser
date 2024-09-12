const S = 2;
const N = 30;

const alphabet =
  [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '2', '3', '4', '5', '6', '1', '7', '8', '9'
  ]

export const translate = (s1) => {
  const s2 = [];
  for (var i = 0; i < s1.length; i++) {
    s2.push((alphabet[(alphabet.indexOf(s1[i]) + S) % alphabet.length]));
  }
  s2.push('1')
  while (s2.length < N) {
    // const r = Math.floor(Math.random() * 10);
    const randomIndex = Math.floor(Math.random() * alphabet.length);
    s2.push(alphabet[randomIndex]);
  }
  return s2.join("");
}

export const untranslate = (s1) => {
  const j = s1.indexOf("1");
  s1 = s1.slice(0, j)
  const s2 = [];
  for (var i = 0; i < s1.length; i++) {
    const c1 = s1[i];
    const c2 = (alphabet[(alphabet.indexOf(c1) - S + alphabet.length) % alphabet.length]);
    s2.push(c2);
  }
  return s2.join("");
}