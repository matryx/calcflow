using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class SimpleButton : MonoBehaviour {

    [SerializeField]
    public AddressListModifier addressModifier;
    [SerializeField]
    InputField textField;

	public void SetName(string name)
    {
        this.name = name;
        textField.text = name;
    }

    public void Remove()
    {
        addressModifier.Remove(textField.text);
    }
}
