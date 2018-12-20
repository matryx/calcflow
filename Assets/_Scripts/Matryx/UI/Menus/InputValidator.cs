using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;

public class InputValidator : MonoBehaviour {
    [SerializeField]
    int minimumLength;
    [SerializeField]
    int maximumLength;

    InputField inputField; 
    public string validationRegex = @"([^\s]{1,})( [^\s]{1,})+|([\w\.]{3,})";
    public bool isValid;

    Color defaultColor;

    // Use this for initialization
    void Start () {
        isValid = false;
        inputField = transform.GetComponent<InputField>();
        inputField.onEndEdit.AddListener(validateInput);

        defaultColor = inputField.image.color;
	}

    public void validateInput(string input)
    {
        isValid = Regex.IsMatch(input, validationRegex);
        isValid &= minimumLength > 0 ? input.Length >= minimumLength : true;
        isValid &= maximumLength > 0 ? input.Length <= maximumLength : true;
        inputField.image.color = isValid ? defaultColor : new Color(defaultColor.r + 0.2f, defaultColor.g / 3f, defaultColor.b / 3f);
    }
}
