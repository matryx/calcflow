using System;
using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;

public class NumberPicker : MonoBehaviour {

    [SerializeField]
    public Button plusButton;
    [SerializeField]
    public Button minusButton;
    [SerializeField]
    public InputField valueField;

    [NonSerialized]
    public float maxValue = 9999999;
    [NonSerialized]
    public float minValue = 0;

    float currentValue = 0;
    public float CurrentValue
    {
        get { return currentValue; }
        set
        {
            currentValue = value;
            valueField.text = currentValue.ToString();
        }
    }

	// Use this for initialization
	void Start () {
        CurrentValue = 0;

        plusButton.onClick.AddListener(incrementValue);
        minusButton.onClick.AddListener(decrementValue);
        valueField.onValueChanged.AddListener(setValue);
	}

    public void incrementValue()
    {
        if (currentValue < maxValue)
        {
            CurrentValue++;
        }
    }

    public void decrementValue()
    {
        if (currentValue > minValue)
        {
            CurrentValue--;
        }
    }

    public void setValue(string value)
    {
        if(value.Length > 7)
        {
            CurrentValue = currentValue;
            return;
        }

        var floatRegex = @"^\d+(\.\d+)?$";
        var emptyDecimalRegex = @"^\d+\.$";
        bool isNumber = Regex.IsMatch(value, floatRegex);
        bool isEmptyDecimal = Regex.IsMatch(value, emptyDecimalRegex);
        if(isEmptyDecimal)
        {
            currentValue = (float)Convert.ToDecimal(value);
        }
        else if(value.Equals(""))
        {
            CurrentValue = 0;
        }
        else if(currentValue == 0)
        {
            string trimmedValue = value.Trim('0');
            if (trimmedValue == "")
            {
                CurrentValue = 0;
            }
            else
            {
                CurrentValue = (float)Convert.ToDecimal(value.Trim('0'));
            }
        }
        else if(isNumber)
        {
            CurrentValue = (float)Convert.ToDecimal(value);
        }
        else
        {
            CurrentValue = currentValue;
        }
    }
}
