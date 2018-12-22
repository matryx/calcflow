using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MatryxKeyboardButton : QuickButton {

    [SerializeField]
    GameObject secondaryMenu;
    [SerializeField]
    RayCastButton matryxButton;

    protected override void Start()
    {
        base.Start();
        if(!UnityEngine.SceneManagement.SceneManager.GetActiveScene().name.Contains("FreeParametrization"))
        {
            Debug.Log("Destroying Matryx Menu...");
            Destroy(gameObject);
        }
        else
        {
            Debug.Log("Keeping Matryx Menu...");
        }
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        StartCoroutine(timedPressButtons());
    }

    protected override void ButtonExitBehavior(GameObject other)
    {

    }

    protected IEnumerator timedPressButtons()
    {
        if(!secondaryMenu.activeSelf)
        {
            StartCoroutine(secondaryMenu.GetComponent<ExpandContract>().Expand());
        }

        matryxButton.PressButton(gameObject);
        yield break;
    }
}