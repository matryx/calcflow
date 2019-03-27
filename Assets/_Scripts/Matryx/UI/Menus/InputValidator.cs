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

    public bool isValid;

    Color defaultColor;

    // Use this for initialization
    void Start()
    {
        isValid = false;
        inputField = transform.GetComponent<InputField>();
        inputField.onEndEdit.AddListener(validateInput);

        validationRegexes = new Dictionary<string, string>();
        validationRegexes.Add("private key", @"(0x)[0-9A-Fa-f]{64}");
        validationRegexes.Add("mnemonic", @"^[\w+\s?]+");
        validationRegexes.Add("password", @".+");
        defaultColor = inputField.image.color;
    }

    public void setValidation(string vType)
    {
        validationType = vType;
    }

    public void validateInput(string input)
    {
        isValid = Regex.IsMatch(input, validationRegexes[validationType]);
        isValid &= minimumLength > 0 ? input.Length >= minimumLength : true;
        isValid &= maximumLength > 0 ? input.Length <= maximumLength : true;
        inputField.image.color = isValid ? defaultColor : new Color(defaultColor.r + 0.2f, defaultColor.g / 3f, defaultColor.b / 3f);
    }
}
