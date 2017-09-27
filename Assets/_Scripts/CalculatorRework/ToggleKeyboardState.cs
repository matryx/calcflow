using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleKeyboardState : QuickButton {
    private Transform calculatorPanel, letterPanel;
    private TextMesh stateText;

    //TODO: test this and write script for toggling capital letters

	protected override void Start ()
    {
        base.Start();

        Initialize();
	}
	
    private void Initialize()
    {
        calculatorPanel = transform.parent.Find("KeyboardPanel");
        letterPanel = transform.parent.Find("LetterPanel");

        stateText = transform.parent.GetComponentInChildren<TextMesh>();
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (calculatorPanel.gameObject.activeSelf)
        {
            calculatorPanel.gameObject.SetActive(false);
            letterPanel.gameObject.SetActive(true);

            stateText.text = "123";
        }
        else
        {
            calculatorPanel.gameObject.SetActive(true);
            letterPanel.gameObject.SetActive(false);

            stateText.text = "Abc";
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {

    }

    void Update () { }
}
