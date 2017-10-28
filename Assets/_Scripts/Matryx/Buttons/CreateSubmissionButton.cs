using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CreateSubmissionButton : QuickButton {

    [SerializeField]
    private CreateSubmissionMenu canvasSubmissionMenu;

    protected override void ButtonEnterBehavior(GameObject other)
    {
        canvasSubmissionMenu.gameObject.SetActive(!canvasSubmissionMenu.gameObject.activeSelf);
    }

    protected override void ButtonExitBehavior(GameObject other) {}
}
