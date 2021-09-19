module AlphabetAlphabetCipher.Api.Tests

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let CanEncodeAMessageWithASecretKeyword () =
    let actual = AlphabetCipher.Api.encode "vigilance" "meetmeontuesdayeveningatseven"
    let expected = "hmkbxebpxpmyllyrxiiqtoltfgzzv"
    Assert.That(actual, Is.EqualTo(expected))

    let actual = AlphabetCipher.Api.encode "scones" "meetmebythetree"
    let expected = "egsgqwtahuiljgs"
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let CanDecodeAMessageGivenAnEncodedMessageAndASecretKeyword () =   
    let actual = AlphabetCipher.Api.decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv"
    let expected = "meetmeontuesdayeveningatseven"
    Assert.That(actual, Is.EqualTo(expected))

    let actual = AlphabetCipher.Api.decode "scones" "egsgqwtahuiljgs"
    let expected = "meetmebythetree"
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let CanExtractTheSecretKeywordGivenAndEncryptedMessageAndTheOriginalMessage () =
    let actual = AlphabetCipher.Api.decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog"
    let expected = "vigilance"
    Assert.That(actual, Is.EqualTo(expected))

    let actual = AlphabetCipher.Api.decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs"
    let expected = "scones"
    Assert.That(actual, Is.EqualTo(expected))

    let actual = AlphabetCipher.Api.decipher "hfnlphoontutufa" "hellofromrussia"
    let expected = "abcabcx"
    Assert.That(actual, Is.EqualTo(expected))


