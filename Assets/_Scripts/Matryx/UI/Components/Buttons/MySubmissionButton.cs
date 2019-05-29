using UnityEngine;
using TMPro;

public class MySubmissionButton : QuickButton
{

    [SerializeField]
    TMPro.TextMeshPro text;
    [SerializeField]
    private MySubmissionsMenu mySubmissionsMenu;
    [SerializeField]
    private AnimationHandler mySubmissionsAnimationHandler;
    [SerializeField]
    private FlexButtonComponent submissionButtonFlexComponent;

    private float defaultFontSize = 1.1f;
    private float otherFontSize = 1.1f;
    
    protected override void ButtonEnterBehavior(GameObject other)
    {
        mySubmissionsAnimationHandler.OpenMenu();
        mySubmissionsMenu.LoadMySubmissions(TournamentMenu.Tournament);
        // Select the button
        submissionButtonFlexComponent.SetState(2);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        // Deselect the button
        submissionButtonFlexComponent.SetState(1);
    }
}
