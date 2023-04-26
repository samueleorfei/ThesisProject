namespace Utils

module Word =
    let split (word: string, rules: char array): string array = word.Split(rules);

    let tokenize (word: string): char list = Seq.toList word;
