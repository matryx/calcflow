using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEngine;

public class ImportSubmission : QuickButton {

    public Matryx_Submission submission;

    [SerializeField]
    private CalcManager calcManager;
    [SerializeField]
    private FlexButtonComponent submissionButtonFlexComponent;

    protected override void ButtonEnterBehavior(GameObject other)
    {
        // Select the button
        submissionButtonFlexComponent.SetState(2);

        List<ExpressionSet> ess = JsonHelper.FromJson<SerializableExpressionSet>(submission.body).Select(x => x.ConvertToExpressionSet()).ToList();
        calcManager.LoadSavedExpressionSets(ess);
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        // Deselect the button
        submissionButtonFlexComponent.SetState(1);
    }

    public void Disable()
    {
        submissionButtonFlexComponent.SetState(0);
    }

    public void Reenable()
    {
        submissionButtonFlexComponent.SetState(1);
    }
}
