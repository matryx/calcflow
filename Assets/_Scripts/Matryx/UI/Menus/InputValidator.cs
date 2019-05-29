using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;

public class InputValidator : MonoBehaviour
{
    [SerializeField]
    int minimumLength;
    [SerializeField]
    int maximumLength;

    InputField inputField;
    Dictionary<string, string> validationRegexes;
    public string validationType;

    public static string INPUT_VALIDATION_PRIVATE_KEY = "private key";
    public static string INPUT_VALIDATION_MNEMONIC_PHRASE = "mnemonic";
    public static string INPUT_VALIDATION_PASSWORD = "password";
    public static string INPUT_VALIDATION_KEYSTORE_FILE = "keystore";
    public static string INPUT_VALIDATION_TITLE = "title";
    public static string INPUT_VALIDATION_NONE = "none";

    public bool isValid;

    Color defaultColor;

    // Use this for initialization
    void Start()
    {
        isValid = false;
        inputField = transform.GetComponent<InputField>();
        inputField.onEndEdit.AddListener(endEditListener);

        validationRegexes = new Dictionary<string, string>();
        validationRegexes.Add(INPUT_VALIDATION_PRIVATE_KEY, @"(0x)[0-9A-Fa-f]{64}");
        validationRegexes.Add(INPUT_VALIDATION_MNEMONIC_PHRASE, @"^[\w+\s?]+");
        validationRegexes.Add(INPUT_VALIDATION_PASSWORD, @".+");
        validationRegexes.Add(INPUT_VALIDATION_KEYSTORE_FILE, @".+");
        validationRegexes.Add(INPUT_VALIDATION_TITLE, @"([0-9A-Za-z\-_]+\s?)+");
        validationRegexes.Add(INPUT_VALIDATION_NONE, @".+");
        defaultColor = inputField.image.color;
    }

    public void setValidationType(string vType)
    {
        validationType = vType;
    }

    public void endEditListener(string input)
    {
        validateInput(input);

        if (!isValid)
        {
            inputField.onValueChanged.RemoveAllListeners();
            inputField.onValueChanged.AddListener(validateInput);
        }
    }

    public void validateInput(string input)
    {
        isValid = Regex.IsMatch(input, validationRegexes[validationType]);
        isValid &= minimumLength > 0 ? input.Length >= minimumLength : true;
        isValid &= maximumLength > 0 ? input.Length <= maximumLength : true;
        inputField.image.color = isValid ? defaultColor : new Color(defaultColor.r + 0.2f, defaultColor.g / 3f, defaultColor.b / 3f);

        if(isValid) inputField.onValueChanged.RemoveAllListeners();
    }
}
